using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using CodeExerciseLibrary.SourceGenerator.Extensions;
using CodeExerciseLibrary.SourceGenerator.Helpers;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.FindSymbols;
using Microsoft.CodeAnalysis.FlowAnalysis;
using Microsoft.CodeAnalysis.Text;

namespace CodeExerciseLibrary.SourceGenerator
{
    [Generator]
    public class TestGenerator : ISourceGenerator
    {
        private HashSet<string> GeneratedMethods;

        public TestGenerator()
        {
            this.GeneratedMethods = new HashSet<string>();
        }

        public void Initialize(InitializationContext context)
        {
            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }

        public void Execute(SourceGeneratorContext context)
        {
            if (!(context.SyntaxReceiver is SyntaxReceiver receiver))
            {
                return;
            }

            Compilation compilation = context.Compilation;

            foreach (ClassDeclarationSyntax @class in receiver.Classes)
            {
                NamespaceDeclarationSyntax @namespace = @class.FindParent<NamespaceDeclarationSyntax>();
                if (@namespace is null)
                {
                    continue;
                }

                foreach (MemberDeclarationSyntax member in @class.Members)
                {
                    if (!(member is MethodDeclarationSyntax method))
                    {
                        continue;
                    }

                    if (method.Body is null)
                    {
                        continue;
                    }

                    foreach (StatementSyntax statement in method.Body.Statements)
                    {
                        this.ProcessStatement(context, ref compilation, @namespace, @class, statement, null);
                    }
                }
            }

            foreach (LambdaExpressionSyntax lambda in receiver.Lambda)
            {
                NamespaceDeclarationSyntax @namespace = lambda.FindParent<NamespaceDeclarationSyntax>();
                if (@namespace is null)
                {
                    continue;
                }

                ClassDeclarationSyntax @class = lambda.FindParent<ClassDeclarationSyntax>();

                switch (lambda.Body)
                {
                    case BlockSyntax block:
                    {
                        foreach (StatementSyntax statementSyntax in block.Statements)
                        {
                            if (statementSyntax is LocalDeclarationStatementSyntax localDeclaration)
                            {
                                this.ProcessLocalDeclaration(context, ref compilation, @namespace, @class, localDeclaration);

                                foreach (VariableDeclaratorSyntax variableDeclaration in localDeclaration.Declaration.Variables)
                                {
                                    if (variableDeclaration.Initializer?.Value is InvocationExpressionSyntax invocation)
                                    {
                                        this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, localDeclaration.Declaration.Type);
                                    }
                                }

                                continue;
                            }

                            this.ProcessStatement(context, ref compilation, @namespace, @class, statementSyntax, null);
                        }

                        break;
                    }
                    case InvocationExpressionSyntax invocation:
                        this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, null);
                        break;
                }
            }
        }

        private void ProcessStatement(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, StatementSyntax statement, TypeSyntax returnType)
        {
            switch (statement)
            {
                case LocalDeclarationStatementSyntax declaration:
                {
                    this.ProcessLocalDeclaration(context, ref compilation, @namespace, @class, declaration);
                    break;
                }
                case ExpressionStatementSyntax expression:
                {
                    if (!(expression.Expression is InvocationExpressionSyntax invocation))
                    {
                        return;
                    }

                    this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, returnType);
                    break;
                }
            }
        }

        private void ProcessLocalDeclaration(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, LocalDeclarationStatementSyntax declaration)
        {
            SemanticModel methodModel = compilation.GetSemanticModel(declaration.SyntaxTree);
            SymbolInfo targetSymbol = methodModel.GetSymbolInfo(declaration.Declaration.Type);

            //If the declaration type doesn't exist, create it
            if (!(targetSymbol.Symbol is null))
            {
                return;
            }

            string className = declaration.Declaration.Type.ToString();

            SyntaxToken classIdentifier = SyntaxFactory.Identifier(className);

            ClassDeclarationSyntax declarationClass = SyntaxFactory.ClassDeclaration(
                identifier: classIdentifier, 
                modifiers: SyntaxTokenList.Create(SyntaxFactory.Token(SyntaxKind.PublicKeyword)),

                attributeLists: default,
                typeParameterList: default,
                baseList: default,
                constraintClauses: default,
                members: default
            );

            NamespaceDeclarationSyntax newNamespace = SyntaxFactory.NamespaceDeclaration(
                name: @namespace.Name,
                members: new SyntaxList<MemberDeclarationSyntax>(declarationClass),

                externs: default,
                usings: default
            );

            CompilationUnitSyntax compilationUnit = SyntaxFactory.CompilationUnit(
                members: new SyntaxList<MemberDeclarationSyntax>(newNamespace),

                externs: default,
                usings: default,
                attributeLists: default
            );

            CSharpCompilation csharpCompilation = CSharpCompilation.Create(
                assemblyName: "GeneratedClass",
                syntaxTrees: ImmutableList.Create(compilationUnit.SyntaxTree),

                references: new[]
                {
                    MetadataReference.CreateFromFile(typeof(object).GetTypeInfo().Assembly.Location),
                },

                options: new CSharpCompilationOptions(outputKind: OutputKind.DynamicallyLinkedLibrary)
            );

            static byte[] Emit(CSharpCompilation compilation)
            {
                using MemoryStream memoryStream = new MemoryStream();

                compilation.Emit(memoryStream);

                return memoryStream.ToArray();
            }

            MetadataReference metadataReference = MetadataReference.CreateFromImage(Emit(csharpCompilation));

            //Add the new generated class reference to the global compilation context
            compilation = compilation.AddReferences(metadataReference);

            StringBuilder argumentList = new StringBuilder(0);

            string generatedMissingClass = CSharpMemberGenerator.GetEmptyClass(newNamespace, className, argumentList);

            SourceText generatedClass = SourceText.From(generatedMissingClass, Encoding.UTF8);

            context.AddSource($"{className}.cs", generatedClass);
        }

        private void ProcessInvocation(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, InvocationExpressionSyntax invocation, TypeSyntax returnType)
        {
            foreach (ArgumentSyntax argument in invocation.ArgumentList.Arguments)
            {
                if (argument.Expression is InvocationExpressionSyntax argumentInvocation)
                {
                    TypeSyntax argumentReturnType = SyntaxFactory.ParseTypeName("dynamic");

                    this.ProcessInvocation(context, ref compilation, @namespace, @class, argumentInvocation, argumentReturnType);
                }
            }

            if (!(invocation.Expression is MemberAccessExpressionSyntax invokeMember))
            {
                return;
            }

            //Skip any generic method
            if (invokeMember.DescendantNodes().OfType<GenericNameSyntax>().Any())
            {
                return;
            }
            
            IdentifierNameSyntax targetIdentifier = invokeMember.DescendantNodes().OfType<IdentifierNameSyntax>().LastOrDefault();
            if (targetIdentifier is null)
            {
                return;
            }

            SemanticModel methodModel = compilation.GetSemanticModel(invocation.SyntaxTree);
            SymbolInfo targetSymbol = methodModel.GetSymbolInfo(targetIdentifier);

            //If we are not sure about the symbol we should just ditch without breaking everything by accident!
            if (targetSymbol.CandidateReason != CandidateReason.None)
            {
                return;
            }

            SymbolInfo methodSymbolInfo = methodModel.GetSymbolInfo(invocation);

            //If there's no symbol for target then it doesn't exists
            if (targetSymbol.Symbol is null)
            {
                IdentifierNameSyntax last = null;
                foreach (IdentifierNameSyntax node in invokeMember.DescendantNodes().OfType<IdentifierNameSyntax>())
                {
                    if (node == targetIdentifier)
                    {
                        break;
                    }

                    last = node;
                }

                if (!(last is null))
                {
                    targetSymbol = methodModel.GetSymbolInfo(last);
                }
            }

            //If there's no symbol for method then it doesn't exists
            if (!(methodSymbolInfo.Symbol is null))
            {
                return;
            }

            bool isStatic = false;

            ITypeSymbol extendingClass;
            switch (targetSymbol.Symbol)
            {
                case ITypeSymbol typeSymbol:
                    extendingClass = typeSymbol;

                    isStatic = true;
                    break;
                case ILocalSymbol localSymbol:
                    extendingClass = localSymbol.Type;
                    break;
                case IMethodSymbol methodSymbol:
                    extendingClass = methodSymbol.ReturnType;
                    break;
                default:
                    return;
            }

            StringBuilder argumentList = new StringBuilder(0);

            foreach (ArgumentSyntax argument in invocation.ArgumentList.Arguments)
            {
                if (argumentList.Length > 0)
                {
                    argumentList.Append(", ");
                }

                //Using dynamic should work on all cases
                argumentList.Append($"dynamic generated{argumentList.Length}");
            }

            this.GenerateMethod(context, isStatic, @namespace, @class, extendingClass, invokeMember, argumentList, returnType == null ? "void" : "dynamic"); //Use dynamic as quick hack for return values
        }

        private void GenerateMethod(SourceGeneratorContext context, bool isStatic, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, StringBuilder argumentList, string returnType)
        {
            string identifier = $"{extendingClass.Name}_{invokeMember.Name}_{argumentList.Length}_{returnType}.cs";
            if (!this.GeneratedMethods.Add(identifier))
            {
                return;
            }

            string generateMissingMethod = isStatic
                ? CSharpMemberGenerator.GetStaticMethod(@namespace, @class, extendingClass, invokeMember, argumentList, returnType)
                : CSharpMemberGenerator.GetInstanceMethod(@namespace, extendingClass, invokeMember, argumentList, returnType);

            SourceText generatedMethod = SourceText.From(generateMissingMethod, Encoding.UTF8);

            context.AddSource(identifier, generatedMethod);
        }

        private class SyntaxReceiver : ISyntaxReceiver
        {
            internal List<ClassDeclarationSyntax> Classes { get; } = new List<ClassDeclarationSyntax>();
            internal List<LambdaExpressionSyntax> Lambda { get; } = new List<LambdaExpressionSyntax>();

            public void OnVisitSyntaxNode(SyntaxNode syntaxNode)
            {
                switch (syntaxNode)
                {
                    case ClassDeclarationSyntax @class:
                        this.Classes.Add(@class);
                        break;
                    case LambdaExpressionSyntax lambda:
                        this.Lambda.Add(lambda);
                        break;
                }
            }
        }
    }
}
