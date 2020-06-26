using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CodeExerciseLibrary.SourceGenerator.Helpers
{
    internal static class CSharpMemberGenerator
    {
        internal static string GetStaticMethod(NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, string arguments, string returnType)
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    public partial class {@class.Identifier}
    {{
        private partial class {extendingClass.Name} : {extendingClass}
        {{
            public static {returnType} {invokeMember.Name}({arguments})
            {{
                throw new NotImplementedException(""You are missing a static method with name {invokeMember.Name} on {extendingClass.Name}!"");
            }}
        }}
    }}
}}";
        }

        internal static string GetInstanceMethod(NamespaceDeclarationSyntax @namespace, ITypeSymbol extendingClass, MemberAccessExpressionSyntax invokeMember, string arguments, string returnType)
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    internal static partial class {extendingClass.Name}Extension
    {{
        public static {returnType} {invokeMember.Name}(this {extendingClass} @this{(arguments.Length > 0 ? $", {arguments}" : "")})
        {{
            throw new NotImplementedException(""You are missing a method with name {invokeMember.Name} on {extendingClass.Name}!"");
        }}
    }}
}}";
        }

        internal static string GetEmptyClass(NamespaceDeclarationSyntax @namespace, string className, string arguments = "")
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    internal partial class {className}
    {{
        internal {className}({arguments})
        {{
            throw new NotImplementedException(""You are missing a class with name {className}!"");
        }}
    }}
}}";
        }

        internal static string GetStaticField(NamespaceDeclarationSyntax @namespace, ClassDeclarationSyntax @class, ITypeSymbol extendingClass, IdentifierNameSyntax targetIdentifer)
        {
            return $@"
using System;
namespace {@namespace.Name}
{{
    public partial class {@class.Identifier}
    {{
        private partial class {extendingClass.Name} : {extendingClass}
        {{
            public static dynamic {targetIdentifer} => throw new NotImplementedException(""You are missing a field with name {targetIdentifer} on {extendingClass.Name}!"");
        }}
    }}
}}";
        }

        internal static string GetArgumentList(SeparatedSyntaxList<ArgumentSyntax> arguments)
        {
            if (arguments.Count <= 0)
            {
                return string.Empty;
            }

            StringBuilder argumentList = new StringBuilder();

            foreach (ArgumentSyntax argument in arguments)
            {
                if (argumentList.Length > 0)
                {
                    argumentList.Append(", ");
                }

                //Using dynamic should work on all cases
                argumentList.Append($"dynamic generated{argumentList.Length}");
            }

            return argumentList.ToString();
        }
    }
}
