// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the MIT license.  See License.txt in the project root for license information.

using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Testing;

using CSTest = Test.Utilities.CSharpCodeFixVerifier<
    Microsoft.NetCore.CSharp.Analyzers.Performance.CSharpUseAlternateKeyLookupAnalyzer,
    Microsoft.NetCore.CSharp.Analyzers.Performance.CSharpPreferLengthCountIsEmptyOverAnyFixer>.Test;

namespace Microsoft.NetCore.Analyzers.Performance.UnitTests;

public class UseAlternateKeyLookupTests
{
    [Fact]
    public async Task WithSubstring()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M()
                {
                    var dict = new Dictionary<string, string>();
                    dict.TryGetValue({|#0:"key".Substring(1)|}, out _);
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithRange()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M()
                {
                    var dict = new Dictionary<string, string>();
                    dict.TryGetValue({|#0:"key"[1..]|}, out _);
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());
        test.LanguageVersion = LanguageVersion.CSharp8;

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithIndexer()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M()
                {
                    var dict = new Dictionary<string, string>();
                    dict[{|#0:"key"[1..]|}] = "";
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());
        test.LanguageVersion = LanguageVersion.CSharp8;

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithLocal()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M()
                {
                    var dict = new Dictionary<string, string>();
                    var key = {|#0:"abc".Substring(1)|};
                    dict[key] = "";
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithSwitch()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M(object obj)
                {
                    switch (obj)
                    {
                        case string key:
                            var dict = new Dictionary<string, string>();
                            key = {|#0:"abc".Trim()|};
                            dict[key] = "";
                            break;
                    }
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithParameter()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M(string key)
                {
                    var dict = new Dictionary<string, string>();
                    key = {|#0:key.Trim()|};
                    dict[key] = "";
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithLocalInPattern()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M(object obj)
                {
                    var dict = new Dictionary<string, string>();
                    if (obj is string key)
                    {
                        key = {|#0:key.Trim()|};
                        dict[key] = "";
                    }
                }
            }
            """;

        var test = CSTest.Create(source, CreateDiagnostic());

        // Act & Assert
        await test.RunAsync();
    }

    [Fact]
    public async Task WithForeach()
    {
        // Arange
        var source = """
            using System.Collections.Generic;

            class Test
            {
                public void M()
                {
                    var dict = new Dictionary<string, string>();
                    foreach (var key in new string[0])
                    {
                        dict[key] = "";
                    }
                }
            }
            """;

        var test = CSTest.Create(source);

        // Act & Assert
        await test.RunAsync();
    }

    private static DiagnosticResult CreateDiagnostic()
    {
        return new DiagnosticResult(UseAlternateKeyLookupAnalyzer.UseAlternateKeyLookupDescriptor).WithLocation(0);
    }
}
