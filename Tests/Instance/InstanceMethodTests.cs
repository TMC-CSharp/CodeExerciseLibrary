﻿using System;
using System.Collections.Generic;
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
    }
}
