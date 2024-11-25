procedure Main is
begin
   begin
      Ada.Text_IO.Put_Line
        ("Hello"
         & ", World!");

   exception
      --  We have caught an exception when trying to create the .als
      --  directory: warn the user.
      when GNATCOLL.VFS.VFS_Directory_Error =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "warning: Could not create default ALS log directory at '"
            & ALS_Dir.Display_Full_Name & "'"
            & Ada.Characters.Latin_1.LF
            & "Please make sure the parent directory is writable or "
            & "specify another parent directory via the ALS_HOME "
            & "environment variable.");
         ALS_Dir := GNATCOLL.VFS.No_File;
   end;
end Main;
