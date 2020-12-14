using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using CodeExerciseLibrary.TestAssembly;
using Xunit;

namespace CodeExerciseLibrary.Tests.Static
{
    public partial class StaticMethodTests
    {
        [Fact]
        public void GenerateStaticVoidMethod()
        {
            Assert.Throws<NotImplementedException>(() => StubClass.StaticVoid());
        }

        [Fact]
        public void GenerateSameForDuplicates()
        {
            Assert.Throws<NotImplementedException>(() => StubClass.StaticDuplicateVoid());
            Assert.Throws<NotImplementedException>(() => StubClass.StaticDuplicateVoid());
        }

        [Fact]
        public void GenerateStaticVoidMethodArgument()
        {
            Assert.Throws<NotImplementedException>(() => StubClass.StaticVoid("First"));
        }

        [Fact]
        public void GenerateStaticVoidMethodArguments()
        {
            Assert.Throws<NotImplementedException>(() => StubClass.StaticVoid("First", "Second", "Third"));
        }

        [Fact]
        public void GenerateStaticLocalDeclarationIntMethod()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                int value = StubClass.StaticReturnInt();
            });
        }

        [Fact]
        public void GenerateMethodUseIntMethodArguments()
        {
            Assert.Throws<NotImplementedException>(() => Use(StubClass.StaticReturnToMethodInt()));

            static void Use(int value)
            {
            }
        }

        [Fact]
        public void GenerateStaticReturnMethod()
        {
            Assert.Throws<NotImplementedException>(() => StubClass.RealStaticReturnStubClass().StaticVoidOnReturn());
        }

        [Fact]
        public void GenerateStaticUseReturnMethod()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                int value = StubClass.RealStaticReturnStubClass().StaticReturnInt2();
            });
        }

        [Fact]
        public void GenerateStaticVoidMethodGenerateArguments()
        {
            Assert.Throws<NotImplementedException>(() => StubClass.StaticVoid(UnknownFieldClass.UnknownField));
        }

        [Fact]
        public void GenerateStaticUsing()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                using (StringWriter textWriter = new())
                {
                    StubClass.StaticVoidInsideUsing();
                }
            });
        }

        [Fact]
        public void GenerateStaticUsingExpression()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                using StringWriter textWriter = StubClass.StaticVoidInsideUsingExpression();
            });
        }

        [Fact]
        public void GenerateStaticBlock()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                {
                    StubClass.StaticVoidInsideBlock();
                }
            });
        }

        [Fact]
        public void GenerateStaticTry()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                try
                {
                    StubClass.StaticVoidInsideTry();
                }
                catch
                {
                    throw;
                }
            });
        }

        [Fact]
        public void GenerateStaticCatch()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                try
                {
                    throw new Exception();
                }
                catch
                {
                    StubClass.StaticVoidInsideCatch();
                }
            });
        }

        [Fact]
        public void GenerateStaticFinally()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                try
                {
                }
                finally
                {
                    StubClass.GenerateStaticFinally();
                }
            });
        }

        [Fact]
        public void GenerateStaticForeach()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                foreach (char c in "joni")
                {
                    StubClass.StaticVoidInsideForeach();
                }
            });
        }

        [Fact]
        public void GenerateStaticFor()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                for (int i = 0; i < 3; i++)
                {
                    StubClass.StaticVoidInsideFor();
                }
            });
        }

        [Fact]
        public void GenerateStaticWhile()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                while (true)
                {
                    StubClass.StaticVoidInsideWhile();

                    break;
                }
            });
        }

        [Fact]
        public void GenerateStaticWhileDo()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                do
                {
                    StubClass.StaticVoidInsideDo();

                    break;
                }
                while (true);
            });
        }

        [Fact]
        public void GenerateStaticChecked()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                checked
                {
                    StubClass.StaticVoidInsideChecked();
                }
            });
        }

        [Fact]
        public void GenerateStaticUnchecked()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                unchecked
                {
                    StubClass.StaticVoidInsideUnchecked();
                }
            });
        }

        [Fact]
        public unsafe void GenerateStaticFixed()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                fixed (char* c = "joni")
                {
                    StubClass.StaticVoidInsideFixed();
                }
            });
        }

        [Fact]
        public unsafe void GenerateStaticUnsafe()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                unsafe
                {
                    StubClass.StaticVoidInsideUnsafe();
                }
            });
        }

        [Fact]
        public void GenerateStaticLock()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                lock (this)
                {
                    StubClass.StaticVoidInsideLock();
                }
            });
        }

        [Fact]
        public void GenerateStaticSwitch()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                int test = 0;
                switch (test)
                {
                    case 0:
                        StubClass.StaticVoidInsideSwitch();
                        break;
                }
            });
        }

        [Fact]
        public void GenerateStaticSwitchExpression()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                int test = 0;

                dynamic value = test switch
                {
                    0 => StubClass.StaticVoidInsideSwitchExpression()
                };
            });
        }
    }
}

