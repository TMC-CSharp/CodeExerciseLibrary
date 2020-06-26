using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Dynamic;
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
    public partial class TestGenerator : ISourceGenerator
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
                NamespaceDeclarationSyntax? @namespace = @class.FindParent<NamespaceDeclarationSyntax>();
                if (@namespace is null)
                {
                    continue;
                }

                foreach (MemberDeclarationSyntax member in @class.Members)
                {
                    if (!(member is MethodDeclarationSyntax method) || method.Body is null)
                    {
                        continue;
                    }

                    this.ProcessSyntax(context, ref compilation, @namespace, @class, method.Body);
                }
            }

            foreach (LambdaExpressionSyntax lambda in receiver.Lambda)
            {
                NamespaceDeclarationSyntax? @namespace = lambda.FindParent<NamespaceDeclarationSyntax>();
                ClassDeclarationSyntax? @class = lambda.FindParent<ClassDeclarationSyntax>();

                if (@namespace is null || @class is null)
                {
                    continue;
                }

                this.ProcessSyntax(context, ref compilation, @namespace, @class, lambda.Body);
            }
        }

        private void ProcessSyntax(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, CSharpSyntaxNode syntax)
        {
            switch (syntax)
            {
                case BlockSyntax block:
                {
                    foreach (StatementSyntax statementSyntax in block.Statements)
                    {
                        this.ProcessStatement(context, ref compilation, @namespace, @class, statementSyntax, null);
                    }

                    break;
                }
                case InvocationExpressionSyntax invocation:
                    this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, null);
                    break;
            }
        }

        private void ProcessStatement(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, StatementSyntax statement, TypeSyntax? returnType)
        {
            switch (statement)
            {
                case LocalDeclarationStatementSyntax declaration:
                {
                    this.ProcessLocalDeclaration(context, ref compilation, @namespace, declaration);

                    foreach (VariableDeclaratorSyntax variableDeclaration in declaration.Declaration.Variables)
                    {
                        switch (variableDeclaration.Initializer?.Value)
                        {
                            case InvocationExpressionSyntax invocation:
                                this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, declaration.Declaration.Type);
                                break;
                            case ObjectCreationExpressionSyntax objectCreation:
                                this.ProcessObjectCreation(context, ref compilation, @namespace, objectCreation);
                                break;
                        }
                    }

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

        private void ProcessObjectCreation(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ObjectCreationExpressionSyntax objectCreation)
        {
            //Ignore constructors without arguments
            if (objectCreation.ArgumentList?.Arguments.Count <= 0)
            {
                return;
            }

            SemanticModel methodModel = compilation.GetSemanticModel(objectCreation.SyntaxTree);
            SymbolInfo targetSymbol = methodModel.GetSymbolInfo(objectCreation.Type);

            //This is used for generating constructors only (for now, at least), type should exist already
            if (targetSymbol.Symbol is null)
            {
                return;
            }

            SemanticModel methodModelOriginal = context.Compilation.GetSemanticModel(objectCreation.SyntaxTree);
            SymbolInfo targetSymbolOriginal = methodModelOriginal.GetSymbolInfo(objectCreation.Type);

            //Make sure we only add constructors for missing classes
            if (!(targetSymbolOriginal.Symbol is null))
            {
                return;
            }

            string className = objectCreation.Type.ToString();
            string arguments = CSharpMemberGenerator.GetArgumentList(objectCreation.ArgumentList!.Arguments);

            string identifier = $"{className}_{arguments.Length}.cs";
            if (!this.GeneratedMethods.Add(identifier))
            {
                return;
            }

            string generateMissingMethod = CSharpMemberGenerator.GetEmptyClass(@namespace, className, arguments);

            SourceText generatedMethod = SourceText.From(generateMissingMethod, Encoding.UTF8);

            context.AddSource(identifier, generatedMethod);
        }

        private void ProcessLocalDeclaration(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, LocalDeclarationStatementSyntax declaration)
        {
            SemanticModel methodModel = compilation.GetSemanticModel(declaration.SyntaxTree);
            SymbolInfo targetSymbol = methodModel.GetSymbolInfo(declaration.Declaration.Type);

            //If the declaration type doesn't exist, create it
            if (!(targetSymbol.Symbol is null))
            {
                return;
            }

            string className = declaration.Declaration.Type.ToString();

            this.GenerateEmptyClass(context, ref compilation, @namespace, className);
        }

        private ITypeSymbol GenerateEmptyClass(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, string className)
        {
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

            string generatedMissingClass = CSharpMemberGenerator.GetEmptyClass(newNamespace, className);

            SourceText generatedClass = SourceText.From(generatedMissingClass, Encoding.UTF8);

            context.AddSource($"{className}.cs", generatedClass);

            return compilation.GetTypeByMetadataName($"{@namespace.Name}.{declarationClass.Identifier}")!;
        }

        private void ProcessArguments(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, SeparatedSyntaxList<ArgumentSyntax> arguments)
        {
            foreach (ArgumentSyntax argument in arguments)
            {
                switch (argument.Expression)
                {
                    case InvocationExpressionSyntax argumentInvocation:
                    {
                        TypeSyntax argumentReturnType = SyntaxFactory.ParseTypeName("dynamic");

                        this.ProcessInvocation(context, ref compilation, @namespace, @class, argumentInvocation, argumentReturnType);
                        break;
                    }
                    case MemberAccessExpressionSyntax memberAccess:
                    {
                        IdentifierNameSyntax targetIdentifier = memberAccess.DescendantNodes().OfType<IdentifierNameSyntax>().LastOrDefault();
                        if (targetIdentifier is null)
                        {
                            continue;
                        }

                        (bool isStatic, ITypeSymbol? extendingClass) = this.ProcessMemberAccess(context, ref compilation, @namespace, memberAccess);
                        if (!isStatic || extendingClass is null)
                        {
                            continue;
                        }

                        this.GenerateStaticField(context, @namespace, @class, extendingClass, targetIdentifier);

                        break;
                    }
                }
            }
        }

        private void ProcessInvocation(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, InvocationExpressionSyntax invocation, TypeSyntax? returnType)
        {
            this.ProcessArguments(context, ref compilation, @namespace, @class, invocation.ArgumentList.Arguments);

            if (!(invocation.Expression is MemberAccessExpressionSyntax memberAccess))
            {
                return;
            }

            (bool isStatic, ITypeSymbol? extendingClass) = this.ProcessMemberAccess(context, ref compilation, @namespace, memberAccess);

            if (extendingClass is null)
            {
                return;
            }

            string arguments = CSharpMemberGenerator.GetArgumentList(invocation.ArgumentList.Arguments);

            this.GenerateMethod(context, isStatic, @namespace, @class, extendingClass, memberAccess, arguments, returnType == null ? "void" : "dynamic"); //Use dynamic as quick hack for return values
        }

        private (bool isStatic, ITypeSymbol? symbol) ProcessMemberAccess(SourceGeneratorContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, MemberAccessExpressionSyntax memberAccess)
        {
            //Skip any generic method
            if (memberAccess.DescendantNodes().OfType<GenericNameSyntax>().Any())
            {
                return default;
            }
            
            IdentifierNameSyntax targetIdentifier = memberAccess.DescendantNodes().OfType<IdentifierNameSyntax>().LastOrDefault();
            if (targetIdentifier is null)
            {
                return default;
            }

            SemanticModel methodModel = compilation.GetSemanticModel(memberAccess.SyntaxTree);
            SymbolInfo targetSymbol = methodModel.GetSymbolInfo(targetIdentifier);

            //If we are not sure about the symbol we should just ditch without breaking everything by accident!
            if (targetSymbol.CandidateReason != CandidateReason.None)
            {
                return default;
            }

            SymbolInfo methodSymbolInfo = methodModel.GetSymbolInfo(memberAccess);

            //If there's no symbol for target then it doesn't exists
            if (targetSymbol.Symbol is null)
            {
                IdentifierNameSyntax? last = null;
                foreach (IdentifierNameSyntax node in memberAccess.DescendantNodes().OfType<IdentifierNameSyntax>())
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

                    if (targetSymbol.Symbol is null)
                    {
                        targetIdentifier = last;
                    }
                }
            }

            //If there's no symbol for method then it doesn't exists
            if (!(methodSymbolInfo.Symbol is null))
            {
                return default;
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
                {
                    //Ehh... Lets assume its a missing static class? Ehheheh...
                    if (targetIdentifier.ToString() == "Test")
                    {
                            return default;
                    }

                    ITypeSymbol symbol = this.GenerateEmptyClass(context, ref compilation, @namespace, targetIdentifier.ToString());

                    return (true, symbol);
                }
            }

            return (isStatic, extendingClass);
        }

        private void GenerateMethod(SourceGeneratorContext context, bool isStatic, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, string arguments, string returnType)
        {
            string identifier = $"{extendingClass.Name}_{invokeMember.Name}_{arguments.Length}_{returnType}.cs";
            if (!this.GeneratedMethods.Add(identifier))
            {
                return;
            }

            string generateMissingMethod = isStatic
                ? CSharpMemberGenerator.GetStaticMethod(@namespace, @class, extendingClass, invokeMember, arguments, returnType)
                : CSharpMemberGenerator.GetInstanceMethod(@namespace, extendingClass, invokeMember, arguments, returnType);

            SourceText generatedMethod = SourceText.From(generateMissingMethod, Encoding.UTF8);

            context.AddSource(identifier, generatedMethod);
        }

        private void GenerateStaticField(SourceGeneratorContext context, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, IdentifierNameSyntax targetIdentifer)
        {
            string identifier = $"{extendingClass.Name}_{targetIdentifer}.cs";
            if (!this.GeneratedMethods.Add(identifier))
            {
                return;
            }

            string generateMissingField = CSharpMemberGenerator.GetStaticField(@namespace, @class, extendingClass, targetIdentifer);

            SourceText generatedField = SourceText.From(generateMissingField, Encoding.UTF8);

            context.AddSource(identifier, generatedField);
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
