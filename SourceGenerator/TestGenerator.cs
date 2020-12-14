using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
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

        public void Initialize(GeneratorInitializationContext context)
        {
            context.RegisterForSyntaxNotifications(() => new SyntaxReceiver());
        }

        public void Execute(GeneratorExecutionContext context)
        {
            if (context.SyntaxReceiver is not SyntaxReceiver receiver)
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
                    if (member is not MethodDeclarationSyntax method || method.Body is null)
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

        private void ProcessSyntax(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, CSharpSyntaxNode syntax)
        {
            switch (syntax)
            {
                case BlockSyntax block:
                    {
                        foreach (StatementSyntax statementSyntax in block.Statements)
                        {
                            this.ProcessSyntax(context, ref compilation, @namespace, @class, statementSyntax);
                        }

                        break;
                    }
                case InvocationExpressionSyntax invocation:
                    this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, null);
                    break;
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
                                    this.ProcessObjectCreation(context, ref compilation, @namespace, @class, objectCreation);
                                    break;
                                case SwitchExpressionSyntax @switch:
                                    foreach (SwitchExpressionArmSyntax switchArm in @switch.Arms)
                                    {
                                        this.ProcessSyntax(context, ref compilation, @namespace, @class, switchArm.Expression);
                                    }
                                    break;
                            }
                        }

                        break;
                    }
                case ExpressionStatementSyntax expression:
                    {
                        if (expression.Expression is not InvocationExpressionSyntax invocation)
                        {
                            return;
                        }

                        this.ProcessInvocation(context, ref compilation, @namespace, @class, invocation, null);
                        break;
                    }
                case UsingStatementSyntax @using:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @using.Statement);
                        break;
                    }
                case TryStatementSyntax @try:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @try.Block);

                        foreach (CatchClauseSyntax @catch in @try.Catches)
                        {
                            this.ProcessSyntax(context, ref compilation, @namespace, @class, @catch.Block);
                        }

                        if (@try.Finally is not null)
                        {
                            this.ProcessSyntax(context, ref compilation, @namespace, @class, @try.Finally);
                        }
                        break;
                    }
                case FinallyClauseSyntax @finally:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @finally.Block);
                        break;
                    }
                case ForEachStatementSyntax @foreach:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @foreach.Statement);
                        break;
                    }
                case ForStatementSyntax @for:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @for.Statement);
                        break;
                    }
                case WhileStatementSyntax @while:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @while.Statement);
                        break;
                    }
                case DoStatementSyntax @do:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @do.Statement);
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @do.Condition);
                        break;
                    }
                case CheckedStatementSyntax @checked:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @checked.Block);
                        break;
                    }
                case FixedStatementSyntax @fixed:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @fixed.Statement);
                        break;
                    }
                case UnsafeStatementSyntax @unsafe:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @unsafe.Block);
                        break;
                    }
                case LockStatementSyntax @lock:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @lock.Statement);
                        break;
                    }
                case SwitchStatementSyntax @switch:
                    {
                        this.ProcessSyntax(context, ref compilation, @namespace, @class, @switch.Expression);

                        foreach (SwitchSectionSyntax section in @switch.Sections)
                        {
                            foreach (StatementSyntax statement in section.Statements)
                            {
                                this.ProcessSyntax(context, ref compilation, @namespace, @class, statement);
                            }
                        }
                        break;
                    }
            }
        }

        private void ProcessObjectCreation(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ObjectCreationExpressionSyntax objectCreation)
        {
            if (!(objectCreation.Initializer is null))
            {
                foreach (ExpressionSyntax initializerExpression in objectCreation.Initializer.Expressions)
                {
                    if (initializerExpression is ObjectCreationExpressionSyntax initializerObjectCreation)
                    {
                        SemanticModel classMethodModel = compilation.GetSemanticModel(initializerObjectCreation.SyntaxTree);
                        SymbolInfo classTargetSymbol = classMethodModel.GetSymbolInfo(initializerObjectCreation.Type);

                        //If the type doesn't exist, create it
                        if (classTargetSymbol.Symbol is null)
                        {
                            this.GenerateEmptyClass(context, ref compilation, @namespace, initializerObjectCreation.Type.ToString());
                        }

                        this.ProcessObjectCreation(context, ref compilation, @namespace, @class, initializerObjectCreation);
                    }
                }
            }

            //Ignore constructors without arguments
            if (objectCreation.ArgumentList is null || objectCreation.ArgumentList.Arguments.Count <= 0)
            {
                return;
            }

            SemanticModel methodModel = compilation.GetSemanticModel(objectCreation.SyntaxTree);
            SymbolInfo targetSymbol = methodModel.GetSymbolInfo(objectCreation.Type);

            this.ProcessArguments(context, ref compilation, @namespace, @class, objectCreation.ArgumentList.Arguments);

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
            string arguments = CSharpMemberGenerator.GetArgumentList(objectCreation.ArgumentList.Arguments);

            string identifier = $"{className}_{arguments.Length}.cs";
            if (!this.GeneratedMethods.Add(identifier))
            {
                return;
            }

            string generateMissingMethod = CSharpMemberGenerator.GetEmptyClass(@namespace, className, arguments);

            SourceText generatedMethod = SourceText.From(generateMissingMethod, Encoding.UTF8);

            context.AddSource(identifier, generatedMethod);
        }

        private void ProcessLocalDeclaration(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, LocalDeclarationStatementSyntax declaration)
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

        private ITypeSymbol GenerateEmptyClass(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, string className)
        {
            if (!this.GeneratedMethods.Add($"{className}.cs"))
            {
                return compilation.GetTypeByMetadataName($"{@namespace.Name}.{className}")!;
            }

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
                using MemoryStream memoryStream = new();

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

        private void ProcessArguments(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, SeparatedSyntaxList<ArgumentSyntax> arguments)
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
                            IdentifierNameSyntax? targetIdentifier = memberAccess.DescendantNodes().OfType<IdentifierNameSyntax>().LastOrDefault();
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

        private void ProcessInvocation(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, InvocationExpressionSyntax invocation, TypeSyntax? returnType)
        {
            this.ProcessArguments(context, ref compilation, @namespace, @class, invocation.ArgumentList.Arguments);

            if (invocation.Expression is not MemberAccessExpressionSyntax memberAccess)
            {
                return;
            }

            (bool isStatic, ITypeSymbol? extendingClass) = this.ProcessMemberAccess(context, ref compilation, @namespace, memberAccess);

            if (extendingClass is null)
            {
                return;
            }

            string arguments = CSharpMemberGenerator.GetArgumentList(invocation.ArgumentList.Arguments);

            this.GenerateMethod(context, isStatic, @namespace, @class, extendingClass, memberAccess, arguments, "dynamic"); //Use dynamic as quick hack for return values
        }

        private (bool isStatic, ITypeSymbol? symbol) ProcessMemberAccess(GeneratorExecutionContext context, ref Compilation compilation, NamespaceDeclarationSyntax @namespace, MemberAccessExpressionSyntax memberAccess)
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
                        ITypeSymbol symbol = this.GenerateEmptyClass(context, ref compilation, @namespace, targetIdentifier.ToString());

                        return (true, symbol);
                    }
            }

            return (isStatic, extendingClass);
        }

        private void GenerateMethod(GeneratorExecutionContext context, bool isStatic, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, string arguments, string returnType)
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

        private void GenerateStaticField(GeneratorExecutionContext context, NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, IdentifierNameSyntax targetIdentifer)
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
            internal List<ClassDeclarationSyntax> Classes { get; } = new();
            internal List<LambdaExpressionSyntax> Lambda { get; } = new();

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
