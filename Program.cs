// Copyright (C) 2019 Dmitry Yakimenko (detunized@gmail.com).
// Licensed under the terms of the MIT license. See LICENCE for details.

using System;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace migrate
{
    using System.Collections.Generic;
    using static SyntaxFactory;

    public class NunitToXunitRewriter: CSharpSyntaxRewriter
    {
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

        public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            var newNode = TryConvertAssertThatIsEqualTo(node);
            if (newNode != null)
                return newNode;

            return base.VisitInvocationExpression(node);
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

        // Converts Assert.That(actual, Is.EqualTo(expected)) to Assert.Equal(expected, actual)
        private SyntaxNode TryConvertAssertThatIsEqualTo(InvocationExpressionSyntax node)
        {
            if (!IsMethodCall(node, "Assert", "That"))
                return null;

            var assertThatArgs = GetCallArguments(node);
            if (assertThatArgs.Length != 2)
                return null;

            var isEqualTo = assertThatArgs[1].Expression;
            if (!IsMethodCall(isEqualTo, "Is", "EqualTo"))
                return null;

            var isEqualToArgs = GetCallArguments(isEqualTo);
            if (isEqualToArgs.Length != 1)
                return null;

            var expected = isEqualToArgs[0];
            var actual = assertThatArgs[0];

            return
                InvocationExpression(
                    MemberAccessExpression(
                        SyntaxKind.SimpleMemberAccessExpression,
                        IdentifierName("Assert"),
                        IdentifierName("Equal")))
                .WithArgumentList(
                    ArgumentList(
                        SeparatedList<ArgumentSyntax>(
                            new SyntaxNodeOrToken[] {expected, Token(SyntaxKind.CommaToken), actual})))
                .NormalizeWhitespace()
                .WithTriviaFrom(node);
        }

        private bool IsMethodCall(ExpressionSyntax node, string objekt, string method)
        {
            var invocation = node as InvocationExpressionSyntax;
            if (invocation == null)
                return false;

            var memberAccess = invocation.Expression as MemberAccessExpressionSyntax;
            if (memberAccess == null)
                return false;

            if ((memberAccess.Expression as IdentifierNameSyntax)?.Identifier.ValueText != objekt)
                return false;

            if (memberAccess.Name.Identifier.ValueText != method)
                return false;

            return true;
        }

        private ArgumentSyntax[] GetCallArguments(ExpressionSyntax node)
        {
            return ((InvocationExpressionSyntax)node).ArgumentList.Arguments.ToArray();
        }
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
    }

    static class Program
    {
        static void ConvertFile(string intputPath, string outputPath)
        {
            var programTree = CSharpSyntaxTree.ParseText(File.ReadAllText(intputPath)).WithFilePath(outputPath);
            var compilation = CSharpCompilation.Create("nunit2xunit", new[] {programTree});
            foreach (var sourceTree in compilation.SyntaxTrees)
            {
                var rewriter = new NunitToXunitRewriter();
                var newSource = rewriter.Visit(sourceTree.GetRoot());
                if (newSource != sourceTree.GetRoot())
                    File.WriteAllText(sourceTree.FilePath, newSource.ToFullString());
            }
        }

        static void FindMatches(string filename, params string[] patterns)
        {
            var programTree = CSharpSyntaxTree.ParseText(File.ReadAllText(filename));
            var compilation = CSharpCompilation.Create("nunit2xunit", new[] { programTree });
            var compiledPatterns = patterns.Select(Ast.Compile);

            foreach (var sourceTree in compilation.SyntaxTrees)
            {
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
        }

        static void Main(string[] args)
        {
            if (args.Length < 1 || args.Length > 2)
            {
                Console.WriteLine("Usage: nunit2xunit input.cs [output.cs]");
                return;
            }

            if (true)
                FindMatches(args[0], "Assert.That(@actual, Is.EqualTo(@expected))",
                                     "Assert.That(@actual, Is.EqualTo(true))",
                                     "Assert.That(@actual, Is.EqualTo(false))",
                                     "Assert.That(@code, Throws._<_>())");
            else
                ConvertFile(args[0], args.Length < 2 ? args[0] : args[1]);
        }
    }
}
