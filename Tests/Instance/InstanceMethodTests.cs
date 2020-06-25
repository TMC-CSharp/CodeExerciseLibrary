using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Text;
using CodeExerciseLibrary.TestAssembly;
using Xunit;

namespace CodeExerciseLibrary.Tests.Instance
{
    public class InstanceMethodTests
    {
        [Fact]
        public void GenerateVoidMethod()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() => stub.Void());
        }

        [Fact]
        public void GenerateVoidMethodArgument()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() => stub.Void("First"));
        }

        [Fact]
        public void GenerateVoidMethodArguments()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() => stub.Void("First", "Second", "Third"));
        }

        [Fact]
        public void GenerateLocalDeclarationIntMethodArguments()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() =>
            {
                int value = stub.ReturnInt();
            });
        }

        [Fact]
        public void GenerateMethodUseIntMethodArguments()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() => Use(stub.ReturnToMethodInt()));

            static void Use(int value)
            {
            }
        }

        [Fact]
        public void GenerateReturnMethod()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() => stub.RealInstanceReturnStubClass().VoidOnReturn());
        }

        [Fact]
        public void GenerateStaticUseReturnMethod()
        {
            StubClass stub = new StubClass();

            Assert.Throws<NotImplementedException>(() =>
            {
                int value = stub.RealInstanceReturnStubClass().ReturnInt2();
            });
        }

        [Fact]
        public void GenerateMissingClass()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                MissingClass missingClass = new MissingClass();
            });
        }

        [Fact]
        public void GenerateMissingClassWithArguments()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                MissingClass missingClass = new MissingClass("First");
            });
        }

        [Fact]
        public void GenerateMissingClassWithMethods()
        {
            Assert.Throws<NotImplementedException>(() =>
            {
                //Don't call the constructor! It throws
                MissingClass2 missingClass = (MissingClass2)RuntimeHelpers.GetUninitializedObject(typeof(MissingClass2));

                missingClass.MissingMethodOnMissingClass();
            });
        }
    }
}
