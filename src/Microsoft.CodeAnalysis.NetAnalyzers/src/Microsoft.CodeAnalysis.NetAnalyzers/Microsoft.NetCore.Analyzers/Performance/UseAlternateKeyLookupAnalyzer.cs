// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the MIT license.  See License.txt in the project root for license information.

using System.Collections.Immutable;
using Analyzer.Utilities;
using Analyzer.Utilities.Lightup;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;
using static Microsoft.NetCore.Analyzers.MicrosoftNetCoreAnalyzersResources;

namespace Microsoft.NetCore.Analyzers.Performance;

/// <summary>
/// 
/// </summary>
[DiagnosticAnalyzer(LanguageNames.CSharp, LanguageNames.VisualBasic)]
public class UseAlternateKeyLookupAnalyzer : DiagnosticAnalyzer
{
    internal const string RuleId = "CA1876";

    internal static readonly DiagnosticDescriptor UseAlternateKeyLookupDescriptor = DiagnosticDescriptorHelper.Create(
        RuleId,
        CreateLocalizableResourceString("UseAlternateKeyLookupTitle"),
        CreateLocalizableResourceString("UseAlternateKeyLookupMessage"),
        DiagnosticCategory.Performance,
        RuleLevel.IdeSuggestion,
        CreateLocalizableResourceString("UseAlternateKeyLookupDescription"),
        isPortedFxCopRule: false,
        isDataflowRule: false
    );

    public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get; } = ImmutableArray.Create(UseAlternateKeyLookupDescriptor);

    public override void Initialize(AnalysisContext context)
    {
        context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);
        context.EnableConcurrentExecution();

        context.RegisterCompilationStartAction(context =>
        {
            var typeProvider = WellKnownTypeProvider.GetOrCreate(context.Compilation);
            var dictionaryType = typeProvider.GetOrCreateTypeByMetadataName(WellKnownTypeNames.SystemCollectionsGenericDictionary2);
            var concurrentDictionaryType = typeProvider.GetOrCreateTypeByMetadataName(WellKnownTypeNames.SystemCollectionsConcurrentConcurrentDictionary2);
            // TODO: FrozenDictionary
            var readOnlySpanOfTType = typeProvider.GetOrCreateTypeByMetadataName(WellKnownTypeNames.SystemReadOnlySpan1);
            var readOnlySpanOfCharType = readOnlySpanOfTType?.Construct(context.Compilation.GetSpecialType(SpecialType.System_Char));

            if (dictionaryType != null || concurrentDictionaryType != null)
            {
                context.RegisterOperationAction(context => AnalyzeInvocation(context, dictionaryType, concurrentDictionaryType, readOnlySpanOfCharType), OperationKind.Invocation);
                context.RegisterOperationAction(context => AnalyzePropertyReference(context, dictionaryType, concurrentDictionaryType, readOnlySpanOfCharType), OperationKind.PropertyReference);
            }
        });
    }

    protected virtual void AnalyzePropertyReference(OperationAnalysisContext context, ITypeSymbol? dictionaryType, ITypeSymbol? concurrentDictionaryType, INamedTypeSymbol? readOnlySpanOfCharType)
    {
    }

    private void AnalyzeInvocation(OperationAnalysisContext context, ITypeSymbol? dictionaryType, ITypeSymbol? concurrentDictionaryType, INamedTypeSymbol? readOnlySpanOfCharType)
    {
        var invocation = (IInvocationOperation)context.Operation;
        var methodName = invocation.TargetMethod.Name;
        var containingType = invocation.TargetMethod.ContainingType;

        // TODO: TryGetDefaultValue?
        if ((methodName == "TryGetValue" || methodName == "ContainsValue") &&
            // TODO: Or inherits?
            (containingType.ConstructedFrom.Equals(dictionaryType, SymbolEqualityComparer.Default) ||
            containingType.ConstructedFrom.Equals(concurrentDictionaryType, SymbolEqualityComparer.Default)) &&
            containingType.TypeArguments[0].SpecialType == SpecialType.System_String)
        {
            var keyArgument = invocation.Arguments[0].Value;
            var expression = FindAllocatingExpression(keyArgument, readOnlySpanOfCharType, context);

            if (expression != null)
            {
                context.ReportDiagnostic(Diagnostic.Create(UseAlternateKeyLookupDescriptor, expression.Syntax.GetLocation()));
            }
        }
    }

    protected IOperation? FindAllocatingExpression(IOperation expression, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context)
    {
        switch (expression)
        {
            case ILocalReferenceOperation localRef:
                return FindAllocatingExpression(expression, localRef.Local, readOnlySpanOfCharType, context);

            case IParameterReferenceOperation paramRef:
                return FindAllocatingExpression(expression, paramRef.Parameter, readOnlySpanOfCharType, context);

            case IInvocationOperation invocation:
                var type = invocation.Instance?.Type;
                if (invocation.TargetMethod.Name is "Substring" or "Remove"
                        or "Trim" or "TrimStart" or "TrimEnd"
                        or "ToUpper" or "ToUpperInvariant" or "ToLower" or "ToLowerInvariant" &&
                    type?.SpecialType == SpecialType.System_String)
                {
                    return invocation;
                }
                else if (invocation.TargetMethod.Name is "ToString" && type?.IsRefLikeType == true && context.Compilation.HasImplicitConversion(type, readOnlySpanOfCharType))
                {
                    return invocation;
                }

                break;

            case IOperation when expression.Kind == OperationKindEx.ImplicitIndexerReference && expression.Type?.SpecialType == SpecialType.System_String:
                return expression;

            case IConditionalOperation conditional:
                var expr = FindAllocatingExpression(conditional.WhenTrue, readOnlySpanOfCharType, context);
                return expr ?? FindAllocatingExpression(conditional.WhenFalse!, readOnlySpanOfCharType, context);

            case ICoalesceOperation coalesce:
                expr = FindAllocatingExpression(coalesce.Value, readOnlySpanOfCharType, context);
                return expr ?? FindAllocatingExpression(coalesce.WhenNull, readOnlySpanOfCharType, context);
        }

        return null;
    }

    protected virtual IOperation? FindAllocatingExpression(IOperation keyExpression, ILocalSymbol local, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context) => null;

    protected virtual IOperation? FindAllocatingExpression(IOperation keyExpression, IParameterSymbol param, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context) => null;

    //protected static void AnalyzeExpression(IOperation expression, INamedTypeSymbol? readOnlySpanOfCharType, OperationAnalysisContext context)
    //{
    //    foreach (var operation in expression.DescendantsAndSelf())
    //    {
    //        switch (operation)
    //        {
    //            case IInvocationOperation inv:
    //                var type = inv.Instance?.Type;
    //                if (inv.TargetMethod.Name is "Substring" or "Remove"
    //                        or "Trim" or "TrimStart" or "TrimEnd"
    //                        or "ToUpper" or "ToUpperInvariant" or "ToLower" or "ToLowerInvariant" &&
    //                    type?.SpecialType == SpecialType.System_String)
    //                {
    //                    context.ReportDiagnostic(Diagnostic.Create(UseAlternateKeyLookupDescriptor, expression.Syntax.GetLocation()));
    //                }
    //                else if (inv.TargetMethod.Name is "ToString" && context.Compilation.HasImplicitConversion(type, readOnlySpanOfCharType))
    //                {
    //                    context.ReportDiagnostic(Diagnostic.Create(UseAlternateKeyLookupDescriptor, expression.Syntax.GetLocation()));
    //                }

    //                break;

    //            case IOperation when operation.Kind == OperationKindEx.ImplicitIndexerReference:
    //                context.ReportDiagnostic(Diagnostic.Create(UseAlternateKeyLookupDescriptor, expression.Syntax.GetLocation()));
    //                break;

    //            case ILocalReferenceOperation localRef:
    //                var syntaxRef = localRef.Local.DeclaringSyntaxReferences.First();
    //                var syntax = syntaxRef.GetSyntax();
    //                var block = expression.SemanticModel!.GetOperation(syntax)?.GetAncestor<IBlockOperation>(OperationKind.Block);

    //                if (block != null)
    //                {
    //                    var declarators = block.Descendants().OfType<IVariableDeclaratorOperation>();

    //                    foreach (var declarator in declarators)
    //                    {
    //                        if (localRef.Local.Equals(declarator.Symbol, SymbolEqualityComparer.Default) && declarator.Initializer != null)
    //                        {
    //                            AnalyzeExpression(declarator.Initializer.Value, readOnlySpanOfCharType, context);
    //                        }
    //                    }

    //                    var assignments = block.Descendants().OfType<IAssignmentOperation>();

    //                    foreach (var assignment in assignments)
    //                    {
    //                        if (assignment.Target is ILocalReferenceOperation lr && lr.Local.Equals(localRef.Local, SymbolEqualityComparer.Default))
    //                        {
    //                            AnalyzeExpression(assignment.Value, readOnlySpanOfCharType, context);
    //                        }
    //                    }
    //                }
    //                break;
    //        }
    //    }
    //}
}
