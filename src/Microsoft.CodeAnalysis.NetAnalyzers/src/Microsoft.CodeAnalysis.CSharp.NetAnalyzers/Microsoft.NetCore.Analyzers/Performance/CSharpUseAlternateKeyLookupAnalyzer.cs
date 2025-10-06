// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Analyzer.Utilities.Extensions;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;
using Microsoft.NetCore.Analyzers.Performance;

namespace Microsoft.NetCore.CSharp.Analyzers.Performance;

[DiagnosticAnalyzer(LanguageNames.CSharp)]
public class CSharpUseAlternateKeyLookupAnalyzer : UseAlternateKeyLookupAnalyzer
{
    protected override void AnalyzePropertyReference(OperationAnalysisContext context, ITypeSymbol? dictionaryType, ITypeSymbol? concurrentDictionaryType, INamedTypeSymbol? readOnlySpanOfCharType)
    {
        var operation = context.Operation;
        var elementAccess = operation.Syntax as ElementAccessExpressionSyntax;

        if (elementAccess == null) return;

        var containingType = operation.SemanticModel!.GetTypeInfo(elementAccess.Expression).Type as INamedTypeSymbol;

        if (containingType != null &&
            (containingType.ConstructedFrom.Equals(dictionaryType, SymbolEqualityComparer.Default) ||
            containingType.ConstructedFrom.Equals(concurrentDictionaryType, SymbolEqualityComparer.Default)) &&
            containingType.TypeArguments[0].SpecialType == SpecialType.System_String)
        {
            var keyArgument = operation.SemanticModel!.GetOperation(elementAccess.ArgumentList.Arguments[0].Expression)!;
            var expression = FindAllocatingExpression(keyArgument, readOnlySpanOfCharType, context);

            if (expression != null)
            {
                context.ReportDiagnostic(Diagnostic.Create(UseAlternateKeyLookupDescriptor, expression.Syntax.GetLocation()));
            }
        }
    }

    // Analyze* returning null? 
    protected override IOperation? FindAllocatingExpression(IOperation keyExpression, ILocalSymbol local, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context)
    {
        var declaration = local.DeclaringSyntaxReferences.First().GetSyntax();
        declaration = declaration is VariableDesignationSyntax ? declaration.Parent! : declaration;

        if (declaration is ForEachStatementSyntax)
        {
            return null;
        }

        var declarationOp = keyExpression.SemanticModel!.GetOperation(declaration);
        var block = declarationOp!.GetAncestor<IBlockOperation>(OperationKind.Block);

        if (block == null)
        {
            // TODO
            return null;
        }

        IOperation? expression = null;
        IAssignmentOperation? lastAssignment = null;

        if (declarationOp is IVariableDeclaratorOperation declarator && declarator.Initializer != null)
        {
            expression = FindAllocatingExpression(declarator.Initializer.Value, readOnlySpanOfCharType, context);
        }

        using var enumerator = block.Descendants().GetEnumerator();

        // Rewind to the declaration
        while (enumerator.MoveNext() && enumerator.Current != declarationOp)
        {
        }

        while (enumerator.MoveNext())
        {
            if (enumerator.Current is ILocalReferenceOperation localRef && localRef.Local.Equals(local, SymbolEqualityComparer.Default))
            {
                var assignment = localRef.Parent as IAssignmentOperation;

                if (assignment != null && assignment.Target == localRef)
                {
                    lastAssignment = assignment;
                    expression ??= FindAllocatingExpression(assignment.Value, readOnlySpanOfCharType, context);
                }
                else if (lastAssignment == null || !IsInsideAssignment(localRef, lastAssignment, block))
                {
                    return null;
                }
            }
        }

        return expression;
        //return FindAllocatingExpression(keyExpression, local, readOnlySpanOfCharType, declarationOp!, context);
    }

    private static bool IsInsideAssignment(IOperation operation, IAssignmentOperation assignment, IOperation container)
    {
        var ancestor = operation;
        var insideAssignmentToSelf = false;

        while (ancestor != container)
        {
            if (insideAssignmentToSelf = ancestor == assignment)
            {
                break;
            }

            ancestor = ancestor!.Parent;
        }

        return insideAssignmentToSelf;
    }

    protected override IOperation? FindAllocatingExpression(IOperation keyExpression, IParameterSymbol param, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context)
    {
        var declaration = param.DeclaringSyntaxReferences.First().GetSyntax();
        var bodyOp = keyExpression.GetAncestor<IMethodBodyOperation>(OperationKind.MethodBody);

        if (bodyOp == null)
        {
            // TODO
            return null;
        }

        IOperation? expression = null;
        IAssignmentOperation? lastAssignment = null;

        using var enumerator = bodyOp.Descendants().GetEnumerator();

        while (enumerator.MoveNext())
        {
            if (enumerator.Current is IParameterReferenceOperation paramRef && paramRef.Parameter.Equals(param, SymbolEqualityComparer.Default))
            {
                var assignment = paramRef.Parent as IAssignmentOperation;

                if (assignment != null && assignment.Target == paramRef)
                {
                    lastAssignment = assignment;
                    expression ??= FindAllocatingExpression(assignment.Value, readOnlySpanOfCharType, context);
                }
                else if (lastAssignment == null || !IsInsideAssignment(paramRef, lastAssignment, bodyOp))
                {
                    return null;
                }
            }
        }

        return expression;
        //return FindAllocatingExpression(keyExpression, param, readOnlySpanOfCharType, bodyOp!.BlockBody!, context);
    }

    private IOperation? FindAllocatingExpression(IOperation keyExpression, ISymbol localOrParam, INamedTypeSymbol? readOnlySpanOfCharType, IOperation terminalStatement, OperationAnalysisContext context)
    {
        IOperation? expression = null;
        IOperation? ancestor = keyExpression.Parent;
        IOperation? statement = null;

        while (ancestor != null)
        {
            switch (ancestor)
            {
                case IBlockOperation block:
                    var operations = block.Operations;
                    var index = operations.IndexOf(statement!);
                    var statements = operations.AsSpan().Slice(0, index);

                    if (!AnalyzeStatements(statements, terminalStatement, localOrParam, readOnlySpanOfCharType, context, out expression))
                    {
                        return expression;
                    }
                    break;
                case ISwitchCaseOperation switchCase:
                    operations = switchCase.Body;
                    index = operations.IndexOf(statement!);
                    statements = operations.AsSpan().Slice(0, index);

                    if (!AnalyzeStatements(statements, terminalStatement, localOrParam, readOnlySpanOfCharType, context, out expression))
                    {
                        return expression;
                    }
                    break;
                case IOperation stmt when stmt.Syntax is StatementSyntax:
                    statement = stmt;
                    if (statement == terminalStatement)
                    {
                        return expression;
                    }
                    break;

                // Delegate or local function?
            }

            ancestor = ancestor.Parent;
        }

        return null;
    }

    private bool AnalyzeStatement(IOperation statement, IOperation terminalStatement, ISymbol localOrParam, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context, out IOperation? expression)
    {
        expression = null;

        switch (statement)
        {
            case IConditionalOperation conditional:

                break;

            case ISwitchOperation @switch:
                break;

            case IBlockOperation block:
                AnalyzeStatements(block.Operations.AsSpan(), terminalStatement, localOrParam, readOnlySpanOfCharType, context, out expression);
                break;

            case IVariableDeclarationOperation localDeclaration:
                return AnalyzeDeclaration(localDeclaration, out expression);

            case IVariableDeclarationGroupOperation group:
                foreach (var declaration in group.Declarations)
                {
                    if (!AnalyzeDeclaration(declaration, out expression))
                    {
                        return false;
                    }
                }

                break;

            case IExpressionStatementOperation exprStatement when
                exprStatement.Operation is IAssignmentOperation assignment &&
                ((assignment.Target is ILocalReferenceOperation localRef &&
                localRef.Local.Equals(localOrParam, SymbolEqualityComparer.Default)) ||
                (assignment.Target is IParameterReferenceOperation paramRef &&
                paramRef.Parameter.Equals(localOrParam, SymbolEqualityComparer.Default))):

                expression = FindAllocatingExpression(assignment.Value, readOnlySpanOfCharType, context);
                break;
        }

        return true;

        bool AnalyzeDeclaration(IVariableDeclarationOperation declaration, out IOperation? expression)
        {
            expression = null;

            foreach (var declarator in declaration.Declarators)
            {
                if (declarator.Symbol.Equals(localOrParam, SymbolEqualityComparer.Default))
                {
                    if (declarator.Initializer != null)
                    {
                        expression = FindAllocatingExpression(declarator.Initializer.Value, readOnlySpanOfCharType, context);
                    }

                    return false;
                }
            }

            return true;
        }
    }

    private bool AnalyzeStatements(ReadOnlySpan<IOperation> statements, IOperation terminalStatement, ISymbol localOrParam, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context, out IOperation? expression)
    {
        expression = null;

        for (var i = statements.Length - 1; i >= 0; i--)
        {
            var statement = statements[i];

            if (!AnalyzeStatement(statement, terminalStatement, localOrParam, readOnlySpanOfCharType, context, out expression))
            {
                return false;
            }

            if (expression != null || statement == terminalStatement)
            {
                return false;
            }
        }

        return true;
    }
}
