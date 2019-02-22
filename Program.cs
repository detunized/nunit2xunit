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
    using static SyntaxFactory;

    public class NunitToXunitRewriter: CSharpSyntaxRewriter
    {
        public override SyntaxNode VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            var newNode = TryConvertAssertThatIsEqualTo(node);
            if (newNode != null)
                return newNode;

            return base.VisitInvocationExpression(node);
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

        static void Main(string[] args)
        {
            if (args.Length < 1)
            {
                Console.WriteLine("Usage: nunit2xunit file.cs ...");
                return;
            }

            foreach (var arg in args)
                ConvertFile(arg, arg + ".xunit.cs");
        }
    }
}
