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
            var delphiCode = @"
procedure TForm1.Button1Click(Sender: TObject);
    begin
    end; 
";

            var expectedCsCode = @"
class TForm1
{
    void Button1Click(Sender: TObject)
    {
    }
};
";
            var target = new DelphiToCsConverter.Converter();
            var actual = target.Convert(delphiCode);

            Assert.AreEqual(expectedCsCode, actual);
        }
        [TestMethod]
        public void Call_Convert_WhenEmptyClass()
        {
            var delphiCode = @"
unit Unit1;

interface

type Name = class;

implementation

end.";

            var expectedCsCode = @"
    public class Name
    {
    }";
            var target = new DelphiToCsConverter.Converter();
            var actual = target.Convert(delphiCode);

            Console.WriteLine(actual);

            actual.Should().Contain(expectedCsCode);
        }
        
    }
}
