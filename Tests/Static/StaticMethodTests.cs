using System;
using System.Collections.Generic;
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
        public void GenerateStaticLocalDeclarationIntMethodArguments()
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
    }
}

