package SomeLib.Logger is
    type Logger is tagged null record;

    procedure Log (Self : Logger; Msg : String);

private

    procedure Write_Line (Self : Logger; Line : String);
    procedure Write (Self : Logger; Text : String);

end SomeLib.Logger;
