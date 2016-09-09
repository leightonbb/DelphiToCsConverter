procedure TForm1.Button1Click(Sender: TObject);
var
   S, T: string;
begin
   Str(1.4:2:1, T);
   S := T + ' rounds to ' + IntToStr(Round(1.4)) + #13#10;
   Str(1.5:2:1, T);
   S := S + T + ' rounds to ' + IntToStr(Round(1.5)) + #13#10;
   Str(-1.4:2:1, T);
   S := S + T + ' rounds to ' + IntToStr(Round(-1.4)) + #13#10;
   Str(-1.5:2:1, T);
   S := S + T + ' rounds to ' + IntToStr(Round(-1.5));
   MessageDlg(S, mtInformation, [mbOk], 0, mbOk);
end;