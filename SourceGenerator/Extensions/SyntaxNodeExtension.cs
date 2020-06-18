using System;
using System.Collections.Generic;
using System.Text;
using Microsoft.CodeAnalysis;

namespace CodeExerciseLibrary.SourceGenerator.Extensions
{
    internal static class SyntaxNodeExtension
    {
        internal static T FindParent<T>(this SyntaxNode node) where T: class
        {
            while (!(node is T))
            {
                if (node is null)
                {
                    return null;
                }

                node = node.Parent;
            }

            return node as T;
        }
    }
}
