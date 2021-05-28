namespace FsGettextUtils
  module MoFile = begin
    type MoFileInfo =
      {originalStringLengthsAndOffsets: (uint32 * uint32) array;
       translatedStringLengthsAndOffsets: (uint32 * uint32) array;}
    type MoString =
      {context: string option;
       singular: string;
       pluralForms: string array;}
      with
        override ToString : unit -> string
      end
    val GetMoInfo : s:System.IO.Stream -> MoFileInfo
    val GetOriginalStrings :
      s:System.IO.Stream * info:MoFileInfo * encoding:System.Text.Encoding ->
        indices:seq<int> -> seq<MoString>
    val GetTranslatedStrings :
      s:System.IO.Stream * info:MoFileInfo * encoding:System.Text.Encoding ->
        indices:seq<int> -> seq<MoString>
    val GetEncodingFromMIMEHeader : h:string -> System.Text.Encoding option
  end

