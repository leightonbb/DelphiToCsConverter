using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FluentAssertions;

namespace DelphiToCsConverter.Tests
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void Call_Convert_WhenSingleMethod()
        {
            TestConversion(@"
unit Unit1;

interface

implementation

procedure TForm1.Button1Click(Sender: TObject);
    begin
    end; 
",

@"
class TForm1
{
    void Button1Click(Sender: TObject)
    {
    }
};
");
        }
        [TestMethod]
        public void Call_Convert_WhenEmptyClassWithUnit1Name()
        {
            TestConversion(@"
unit Unit1;

interface

type Name = class;

implementation

end.",

@"
namespace Unit1
{
    
    
    public class Name
    {
    }
}"
);
        }
        private static void TestConversion(string delphiCode, string expectedCsCode)
        {
            var target = new DelphiToCsConverter.Converter();
            var actual = target.Convert(delphiCode);

            Console.WriteLine(actual);

            actual.Should().Contain(expectedCsCode);
        }

        [TestMethod]
        public void Call_Convert_WhenEmptyClassWithUnit2Name2()
        {
            TestConversion(@"
unit Unit2;

interface

type Name2 = class;

implementation

end.",

@"
namespace Unit2
{
    
    
    public class Name2
    {
    }
}"
);
        }
    }
}
