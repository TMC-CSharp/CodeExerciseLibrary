using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CodeExerciseLibrary.TestAssembly
{
    public class StubClass
    {
        public StubClass RealInstanceReturnStubClass() => this;
        public static StubClass RealStaticReturnStubClass() => new StubClass();
    }
}
