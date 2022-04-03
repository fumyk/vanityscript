-spec func_1(IoDevice) -> ok | {error, Reason} when
    IoDevice :: io_device(),
    Reason :: posix() | badarg | terminated.

func_1(File) when is_pid(File) ->
  file_request(File, datasync).
