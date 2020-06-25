using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CodeExerciseLibrary.SourceGenerator.Helpers
{
    internal static class CSharpMemberGenerator
    {
        internal static string GetStaticMethod(NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, StringBuilder argumentList, string returnType)
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    public partial class {@class.Identifier}
    {{
        private partial class {extendingClass.Name} : {extendingClass}
        {{
            public static {returnType} {invokeMember.Name}({argumentList})
            {{
                throw new NotImplementedException(""You are missing a static method with name {invokeMember.Name} on {extendingClass.Name}!"");
            }}
        }}
    }}
}}";
        }

        internal static string GetInstanceMethod(NamespaceDeclarationSyntax @namespace, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, StringBuilder argumentList, string returnType)
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    internal static partial class {extendingClass.Name}Extension
    {{
        public static {returnType} {invokeMember.Name}(this {extendingClass} @this{(argumentList.Length > 0 ? $", {argumentList}" : "")})
        {{
            throw new NotImplementedException(""You are missing a method with name {invokeMember.Name} on {extendingClass.Name}!"");
        }}
    }}
}}";
        }

        internal static string GetEmptyClass(NamespaceDeclarationSyntax @namespace, string className, StringBuilder argumentList)
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    internal partial class {className}
    {{
        internal {className}({argumentList})
        {{
            throw new NotImplementedException(""You are missing a class with name {className}!"");
        }}
    }}
}}";
        }
    }
}
