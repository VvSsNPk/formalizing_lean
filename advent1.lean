def read_file_end (filename : System.FilePath) :  IO (Option IO.FS.Stream) := do 
let fileExists ← filename.pathExists 
if not fileExists then
  let stderr <- IO.getStderr
  stderr.putStrLn s!"File not found : {filename}"
  pure none
else
  let handle ← IO.FS.Handle.mk filename IO.FS.Mode.read
  pure (some (IO.FS.Stream.ofHandle handle))

def read (filename : System.FilePath) : IO (Array String) :=
  IO.FS.lines filename


def x := read "advent1.txt"
