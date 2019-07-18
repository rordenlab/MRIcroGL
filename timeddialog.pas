unit TimedDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TTimedDialogForm }

  TTimedDialogForm = class(TForm)
    PromptLabel: TLabel;
    CloseFormTimer: TTimer;
    procedure CloseFormTimerTimer(Sender: TObject);
    procedure ShowTimedDialog(Title, Prompt: string; ShowInterval: integer; X: integer = -1; Y: integer = - 1);
  private

  public

  end;

var
  TimedDialogForm: TTimedDialogForm;

implementation

{$R *.lfm}

{ TTimedDialogForm }

procedure TTimedDialogForm.CloseFormTimerTimer(Sender: TObject);
begin
  TimedDialogForm.Close;
end;

procedure TTimedDialogForm.ShowTimedDialog(Title, Prompt: string; ShowInterval: integer; X: integer = -1; Y: integer = - 1);
begin
     if (X < 20) or (Y < 20) then
        Self.Position := poScreenCenter
     else begin
       Self.Position := poDesigned;
       Self.Left := X+20;
       Self.Top := Y+20;
     end;
     CloseFormTimer.Interval := ShowInterval;
     Self.Caption := Title;
     PromptLabel.Caption := Prompt;
     Self.Show;
     CloseFormTimer.Enabled := true;
end;

end.

