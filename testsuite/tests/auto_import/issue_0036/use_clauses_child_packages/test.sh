# This test checks that we don't fully qualify already
# partially-qualified names
auto_import -P test.gpr --source src/main.adb --line 2 --column 12
