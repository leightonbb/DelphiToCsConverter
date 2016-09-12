using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FluentAssertions;
using System.IO;
using System.Reflection;

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

type TForm1 = Class(TObject)
    procedure Button1Click(Sender: TObject);
end;

implementation

procedure TForm1.Button1Click(Sender: TObject);
    begin
    end; 


end.
",

@"
    public class TForm1
    {
        
        private void Button1Click(object Sender)
        {
        }
    }
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
        string ReadResourceText(string resourceName)
        {
            var assembly = Assembly.GetExecutingAssembly();

            var names = assembly.GetManifestResourceNames();

            using (Stream stream = assembly.GetManifestResourceStream(this.GetType(), resourceName))
            using (StreamReader reader = new StreamReader(stream))
            {
                return reader.ReadToEnd();
            }
        }
        [TestMethod]
        [Ignore] // Unfinished. Does not read comments in yet.
        public void Call_Convert_WhenFullClass()
        {
            string delphiCode = ReadResourceText("DelphiExamples.TForm1Unit.pas");
            TestConversion(delphiCode,

@"
// Full Unit code."
);
        }
    }
}
