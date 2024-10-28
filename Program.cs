// Copyright (C) 2019 Dmitry Yakimenko (detunized@gmail.com).
// Licensed under the terms of the MIT license. See LICENCE for details.

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace migrate
{
    using static SyntaxFactory;

    public class NunitToXunitRewriter: CSharpSyntaxRewriter
    {
        public NunitToXunitRewriter((string From, string To)[] findReplace)
        {
            _findReplace = findReplace.Select(x => (From: Ast.Compile(x.From), To: Ast.Compile(x.To))).ToArray();
        }

        public override SyntaxNode VisitUsingDirective(UsingDirectiveSyntax node)
        {
            var newNode = TryConvertUsingNunit(node);
            if (newNode != null)
                return newNode;

            return base.VisitUsingDirective(node);
        }

        public override SyntaxNode VisitAttributeList(AttributeListSyntax node)
        {
            if (ShouldRemoveTestFixture(node))
                return null;

            var newNode = TryConvertTestAttribute(node);
            if (newNode != null)
                return newNode;

            return base.VisitAttributeList(node);
        }

        // Find and replace (only in expression statements)
        public override SyntaxNode VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            foreach (var i in _findReplace)
            {
                var matches = Ast.Match(node.Expression, i.From);
                if (matches != null)
                    return node.Update((ExpressionSyntax)Ast.Replace(i.To, matches), node.SemicolonToken).WithTriviaFrom(node);
            }

            return base.VisitExpressionStatement(node);
        }

        // Converts "using NUnit.Framework" to "using Xunit"
        private SyntaxNode TryConvertUsingNunit(UsingDirectiveSyntax node)
        {
            if (node.Name.ToString() != "NUnit.Framework")
                return null;

            return
                UsingDirective(IdentifierName("Xunit"))
                .NormalizeWhitespace()
                .WithTriviaFrom(node);
        }

        // Checks if the node is "[TestFixture]" and should be removed
        private bool ShouldRemoveTestFixture(AttributeListSyntax node)
        {
            return node.Attributes.Count == 1
                && node.Attributes[0].Name.ToString() == "TestFixture"
                && node.Parent is ClassDeclarationSyntax;
        }

        // Converts "[Test]" to "[Fact]"
        private SyntaxNode TryConvertTestAttribute(AttributeListSyntax node)
        {
            if (node.Attributes.Count != 1)
                return null;

            if (node.Attributes[0].Name.ToString() != "Test")
                return null;

            if (!(node.Parent is MethodDeclarationSyntax))
                return null;

            return
                AttributeList(
                    SingletonSeparatedList<AttributeSyntax>(
                        Attribute(
                            IdentifierName("Fact"))))
                .NormalizeWhitespace()
                .WithTriviaFrom(node);
        }

        private (ExpressionSyntax From, ExpressionSyntax To)[] _findReplace;
    }

    public static class Ast
    {
        private class Matcher
        {
            public readonly Dictionary<string, SyntaxNode> Matches = new Dictionary<string, SyntaxNode>();

            public bool Match(SyntaxNode code, SyntaxNode pattern)
            {
                // A placeholder matches anything
                if (IsPlaceholder(pattern))
                {
                    var name = pattern.ToFullString();
                    if (name != "_")
                        Matches[name] = code;

                    return true;
                }

                if (code.GetType() != pattern.GetType())
                    return false;

                switch (code)
                {
                case ArgumentSyntax c:
                    {
                        var p = (ArgumentSyntax)pattern;
                        return Match(c.Expression, p.Expression);
                    }
                case ArgumentListSyntax c:
                    {
                        var p = (ArgumentListSyntax)pattern;
                        return Match(c.OpenParenToken, p.OpenParenToken)
                            && Match(c.Arguments, p.Arguments)
                            && Match(c.CloseParenToken, p.CloseParenToken);
                    }
                case IdentifierNameSyntax c:
                    {
                        var p = (IdentifierNameSyntax)pattern;
                        return Match(c.Identifier, p.Identifier);
                    }
                case InvocationExpressionSyntax c:
                    {
                        var p = (InvocationExpressionSyntax)pattern;
                        return Match(c.Expression, p.Expression)
                            && Match(c.ArgumentList, p.ArgumentList);
                    }
                case LiteralExpressionSyntax c:
                    {
                        var p = (LiteralExpressionSyntax)pattern;
                        return Match(c.Token, p.Token);
                    }
                case MemberAccessExpressionSyntax c:
                    {
                        var p = (MemberAccessExpressionSyntax)pattern;
                        return Match(c.Expression, p.Expression)
                            && Match(c.Name, p.Name);
                    }
                case GenericNameSyntax c:
                    {
                        var p = (GenericNameSyntax)pattern;
                        return Match(c.Identifier, p.Identifier)
                            && Match(c.TypeArgumentList, p.TypeArgumentList);
                    }
                case TypeArgumentListSyntax c:
                    {
                        var p = (TypeArgumentListSyntax)pattern;
                        return Match(c.LessThanToken, p.LessThanToken)
                            && Match(c.Arguments, p.Arguments)
                            && Match(c.GreaterThanToken, p.GreaterThanToken);
                    }
                default:
                    return false;
                }
            }

            //
            // Private
            //

            private bool Match<T>(SeparatedSyntaxList<T> code, SeparatedSyntaxList<T> pattern) where T : SyntaxNode
            {
                if (code.Count != pattern.Count)
                    return false;

                for (var i = 0; i < code.Count; i++)
                    if (!Match(code[i], pattern[i]))
                        return false;

                return true;
            }

            private bool Match(SyntaxToken code, SyntaxToken pattern)
            {
                if (IsPlaceholder(pattern.Text))
                    return true;

                if (code.ValueText == pattern.ValueText)
                    return true;

                return false;
            }

            private bool IsPlaceholder(SyntaxNode pattern)
            {
                return pattern is IdentifierNameSyntax i
                    && IsPlaceholder(i.Identifier.Text);
            }

            private bool IsPlaceholder(string name)
            {
                if (name == "_" || name.StartsWith("@"))
                    return true;

                return false;
            }
        }

        private class Replacer
        {
            public static SyntaxNode Replace(SyntaxNode template, Dictionary<string, SyntaxNode> variables)
            {
                return new Replacer(variables).Replace(template).NormalizeWhitespace();
            }

            //
            // Private
            //

            private readonly Dictionary<string, SyntaxNode> _variables;

            private Replacer(Dictionary<string, SyntaxNode> variables)
            {
                _variables = variables;
            }

            private SyntaxNode Replace(SyntaxNode template)
            {
                // A placeholder matches anything
                if (IsPlaceholder(template))
                {
                    var name = template.ToFullString();
                    if (_variables.TryGetValue(name, out var v))
                        return v;

                    // No substitution found. Should we throw?
                    return template;
                }

                switch (template)
                {
                case ArgumentSyntax t:
                    return t.Update(t.NameColon,
                                    t.RefKindKeyword,
                                    (ExpressionSyntax)Replace(t.Expression));
                case ArgumentListSyntax t:
                    return t.Update(t.OpenParenToken,
                                    Replace(t.Arguments),
                                    t.CloseParenToken);
                case IdentifierNameSyntax t:
                    return t.Update(Replace(t.Identifier));
                case InvocationExpressionSyntax t:
                    return t.Update((ExpressionSyntax)Replace(t.Expression),
                                    (ArgumentListSyntax)Replace(t.ArgumentList));
                case LiteralExpressionSyntax t:
                    return t.Update(Replace(t.Token));
                case MemberAccessExpressionSyntax t:
                    return t.Update((ExpressionSyntax)Replace(t.Expression),
                                    t.OperatorToken,
                                    (SimpleNameSyntax)Replace(t.Name));
                case GenericNameSyntax t:
                    return t.Update(Replace(t.Identifier),
                                    (TypeArgumentListSyntax)Replace(t.TypeArgumentList));
                case TypeArgumentListSyntax t:
                    return t.Update(t.LessThanToken, Replace(t.Arguments), t.GreaterThanToken);
                default:
                    return template;
                }
            }

            private SeparatedSyntaxList<T> Replace<T>(SeparatedSyntaxList<T> template) where T : SyntaxNode
            {
                var result = template;
                for (int i = 0; i < result.Count; i++)
                {
                    var node = result[i];
                    result = result.RemoveAt(i).Insert(i, (T)Replace(node));
                }

                return result;
            }

            private SyntaxToken Replace(SyntaxToken template)
            {
                // TODO: Check this!
                return template;
            }

            //
            // TODO: Share with Matcher
            //

            private bool IsPlaceholder(SyntaxNode pattern)
            {
                return pattern is IdentifierNameSyntax i
                    && IsPlaceholder(i.Identifier.Text);
            }

            private bool IsPlaceholder(string name)
            {
                if (name == "_" || name.StartsWith("@"))
                    return true;

                return false;
            }
        }

        public static ExpressionSyntax Compile(string pattern)
        {
            return ParseExpression(pattern);
        }

        public static Dictionary<string, SyntaxNode> Match(SyntaxNode code, string pattern)
        {
            return Match(code, Compile(pattern));
        }

        public static Dictionary<string, SyntaxNode> Match(SyntaxNode code, SyntaxNode pattern)
        {
            var matcher = new Matcher();
            return matcher.Match(code, pattern) ? matcher.Matches : null;
        }

        public static SyntaxNode Replace(SyntaxNode template, Dictionary<string, SyntaxNode> variables)
        {
            return Replacer.Replace(template, variables);
        }
    }

    static class Program
    {
        static void ConvertFile(string intputPath, string outputPath, params (string From, string To)[] findReplace)
        {
            var programTree = CSharpSyntaxTree.ParseText(File.ReadAllText(intputPath));
            var compilation = CSharpCompilation.Create("nunit2xunit", new[] {programTree});

            foreach (var sourceTree in compilation.SyntaxTrees)
            {
                var rewriter = new NunitToXunitRewriter(findReplace);
                var newSource = rewriter.Visit(sourceTree.GetRoot());
                if (newSource != sourceTree.GetRoot())
                {
                    var text = newSource.ToFullString();
                    if (outputPath == "-")
                        Console.Write(text);
                    else
                        File.WriteAllText(outputPath, text);
                }
            }
        }

        static void ReplaceMatches(string filename, params (string From, string To)[] patterns)
        {
            var sourceTree = CSharpSyntaxTree.ParseText(File.ReadAllText(filename));
            var compiledPatterns = patterns.Select(x => (From: Ast.Compile(x.From), To: Ast.Compile(x.To)));

            var nodes = sourceTree.GetRoot().DescendantNodes().OfType<ExpressionSyntax>();
            foreach (var p in compiledPatterns)
            {
                foreach (var e in nodes)
                {
                    var matches = Ast.Match(e, p.From);
                    if (matches != null)
                    {
                        var r = Ast.Replace(p.To, matches);
                        Console.WriteLine($"  {e} -> {r}");
                    }
                }
            }
        }

        static void FindMatches(string filename, params string[] patterns)
        {
            var sourceTree = CSharpSyntaxTree.ParseText(File.ReadAllText(filename));
            var compiledPatterns = patterns.Select(Ast.Compile);

            var nodes = sourceTree.GetRoot().DescendantNodes().OfType<ExpressionSyntax>();
            foreach (var p in compiledPatterns)
            {
                Console.WriteLine($"===[ {p} ]===");
                foreach (var e in nodes)
                {
                    var matches = Ast.Match(e, p);
                    if (matches != null)
                    {
                        Console.WriteLine($"  {e}");
                        foreach (var m in matches)
                            Console.WriteLine($"    {m.Key} = {m.Value}");
                    }
                }
            }
        }

        static void Main(string[] args)
        {
            if (args.Length < 1 || args.Length > 2)
            {
                Console.WriteLine("Usage: nunit2xunit input.cs [output.cs]");
                return;
            }

            var asserts = new[]
            {
                ("Assert.That(@actual, Is.EqualTo(true))", "Assert.True(@actual)"),
                ("Assert.That(@actual, Is.True)", "Assert.True(@actual)"),
                ("Assert.That(@actual, Is.EqualTo(false))", "Assert.False(@actual)"),
                ("Assert.That(@actual, Is.False)", "Assert.False(@actual)"),
                ("Assert.That(@actual, Is.EqualTo(@expected))", "Assert.Equal(@expected, @actual)"),
                ("Assert.That(@code, Throws.TypeOf<@type>())", "Assert.Throws<@type>(@code)"),
            };

            // Replace nodes (for testing)
            if (false)
                ReplaceMatches(args[0], ("Assert.That(@actual, Is.EqualTo(@expected))", "Assert.Equal(@expected, @actual)"),
                                        ("Assert.That(@actual, Is.EqualTo(true))", "Assert.True(@actual)"),
                                        ("Assert.That(@actual, Is.EqualTo(false))", "Assert.False(@actual)"));
            // Find nodes (for testing)
            else if (false)
                FindMatches(args[0], "Assert.That(@actual, Is.EqualTo(@expected))",
                                     "Assert.That(@actual, Is.EqualTo(true))",
                                     "Assert.That(@actual, Is.EqualTo(false))",
                                     "Assert.That(@code, Throws._<_>())");
            // The actual conversion
            else
                ConvertFile(args[0], args.Length < 2 ? "-" : args[1], asserts);
        }
    }
}
