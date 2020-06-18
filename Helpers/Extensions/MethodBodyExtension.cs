using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace CodeExerciseLibrary.Helpers.Extensions
{
    public static class MethodBodyExtension
    {
        public static bool HasEnumerator(this MethodBody methodBody)
        {
            IList<ExceptionHandlingClause> exceptionHandlers = methodBody.ExceptionHandlingClauses;

            //Foreach generates hidden finally block
            if (exceptionHandlers.All(handler => handler.Flags != ExceptionHandlingClauseOptions.Finally))
            {
                return false;
            }

            IList<LocalVariableInfo> locals = methodBody.LocalVariables;

            //Foreach uses GetEnumerator() internally
            //Which in most case returns IEnumerator
            if (!locals.Any(local =>
                typeof(IEnumerator).IsAssignableFrom(local.LocalType)))
            {
                return false;
            }

            return true;
        }

        public static bool HasLocal<T>(this MethodBody methodBody)
        {
            IList<LocalVariableInfo> locals = methodBody.LocalVariables;

            if (locals.All(local => local.LocalType != typeof(T)))
            {
                return false;
            }

            return true;
        }

        public static bool HasGenericLocal(this MethodBody methodBody, Type genericType)
        {
            IList<LocalVariableInfo> locals = methodBody.LocalVariables;

            if (!locals.Any(local => local.LocalType.IsGenericType && local.LocalType.GetGenericTypeDefinition() == genericType))
            {
                return false;
            }

            return true;
        }
    }
}
