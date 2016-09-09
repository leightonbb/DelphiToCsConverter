﻿// Full Unit code.
// -----------------------------------------------------------
// You must store this code in a unit called Unit1 with a form
// called Form1 that has an OnCreate event called FormCreate.

unit Unit1;

interface

uses
  SysUtils, Forms, Dialogs;

type
  // Define the classes in this Unit at the very start for clarity
  TForm1 = Class;          // This is a forward class definition

  TFruit = Class(TObject)  // This is an actual class definition :
    // Internal class field definitions - only accessible in this unit
    private
      isRound  : Boolean;
      length   : single;
      width    : single;
      diameter : single;
    // Fields and methods only accessible by this class and descendants
    protected
    // Externally accessible fields and methods
    public
      // 2 constructors - one for round fruit, the other long fruit
      constructor Create(diameter : single);               overload;
      constructor Create(length : single; width : single); overload;
    // Externally accessible and inspectable fields and methods
    published
      // Note that properties must use different names to local defs
      property round : Boolean
        read   isRound;
      property len   : single
        read   length;
      property wide  : single
        read   width;
      property diam  : single
        read   diameter;
  end;                    // End of the TFruit class definition

  // The actual TForm1 class is now defined
  TForm1 = Class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure ShowFruit(fruit : TFruit);
  private
    // No local data
  public
    // Uses just the TForm ancestor class public definitions
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Create a round fruit object
constructor TFruit.Create(diameter: single);
begin
  // Indicate that we have a round fruit, and set its size
  isRound       := true;
  self.diameter := diameter;
end;

// Create a long fruit object
constructor TFruit.Create(length, width: single);
begin
  // Indicate that we have a long fruit, and set its size
  isRound     := false;
  self.length := length;
  self.width  := width;
end;

// Form object - action taken when the form is created
procedure TForm1.FormCreate(Sender: TObject);
var
  apple, banana : TFruit;
begin
  // Let us create our fruit objects
  apple  := TFruit.Create(3.5);
  banana := TFruit.Create(7.0, 1.75);

  // Show details about our fruits
  ShowFruit(apple);
  ShowFruit(banana);
end;

// Show what the characteristics of our fruit are
procedure TForm1.ShowFruit(fruit: TFruit);
begin
  if fruit.round
  then ShowMessage('We have a round fruit, with diam = '+
                   FloatToStr(fruit.diam))
  else
  begin
    ShowMessage('We have a long fruit');
    ShowMessage('    it has length = '+FloatToStr(fruit.len));
    ShowMessage('    it has width  = '+FloatToStr(fruit.wide));
  end;
end;

end.