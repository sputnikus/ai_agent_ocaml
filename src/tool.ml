open Message

type result = string

type t = {
  name : string;
  description : string;
  schema : Yojson.Safe.t; (* JSON Schema or example input *)
  run : Yojson.Safe.t -> string Lwt.t; (* JSON input → result *)
}

let time_tool =
  {
    name = "time";
    description = "Returns the current time.";
    schema = `Null;
    (* No parameters needed *)
    run =
      (fun _ ->
        let time = Unix.localtime (Unix.time ()) in
        let formatted =
          Printf.sprintf "It is currently %02d:%02d:%02d." time.tm_hour
            time.tm_min time.tm_sec
        in
        Lwt.return formatted);
  }

let math_tool =
  {
    name = "math";
    description =
      "Performs basic math operations. The expected input format is 'number \
       operator number'. Supported operators are +, -, *, and /. ";
    schema =
      `Assoc
        [
          ("type", `String "object");
          ( "expression",
            `Assoc
              [
                ("type", `String "string");
                ( "description",
                  `String "A binary math expression like, e.g. 2 + 2" );
              ] );
        ];
    run =
      (fun args_json ->
        match args_json with
        | `Assoc [ ("expression", `String expr) ] -> (
            match String.split_on_char ' ' expr with
            | [ a; "+"; b ] ->
                Lwt.return (string_of_int (int_of_string a + int_of_string b))
            | [ a; "-"; b ] ->
                Lwt.return (string_of_int (int_of_string a - int_of_string b))
            | [ a; "*"; b ] ->
                Lwt.return (string_of_int (int_of_string a * int_of_string b))
            | [ a; "/"; b ] ->
                Lwt.return (string_of_int (int_of_string a / int_of_string b))
            | _ -> Lwt.return "Invalid math expression")
        | _ -> Lwt.return "Invalid JSON argument");
  }

let ls_tool =
  {
    name = "ls";
    description = "Lists the contents of the current directory.";
    schema = `Null;
    (* No parameters needed *)
    run =
      (fun _ ->
        let%lwt entries =
          Lwt_stream.to_list (Lwt_unix.files_of_directory ".")
        in
        let files =
          entries |> List.filter (fun name -> name <> "." && name <> "..")
        in
        Lwt.return (String.concat "\n" files));
  }

let read_tool =
  {
    name = "read";
    description = "Reads the contents of a file.";
    schema =
      `Assoc
        [
          ("type", `String "object");
          ( "filename",
            `Assoc
              [
                ("type", `String "string");
                ("description", `String "Path to a file, e.g. filename.txt");
              ] );
        ];
    run =
      (fun args_json ->
        match args_json with
        | `Assoc [ ("filename", `String filename) ] ->
            if
              (* Traverse sanitization *)
              String.contains filename '/' || filename = ""
            then Lwt.return "[Security error: invalid file path]"
            else
              Lwt.catch
                (fun () ->
                  let%lwt content =
                    Lwt_io.with_file ~mode:Lwt_io.input filename Lwt_io.read
                  in
                  Lwt.return content)
                (fun _ -> Lwt.return "[read failed]")
        | _ -> Lwt.return "Invalid JSON argument");
  }

let edit_tool =
  {
    name = "edit";
    description =
      "Make edits to a text file. Finds all instances of the 'target' string \
       and replaces them with the 'replacement'. If the file specified with \
       filename doesn't exist, it will be created.";
    schema =
      `Assoc
        [
          ("type", `String "object");
          ( "filename",
            `Assoc
              [
                ("type", `String "string");
                ("description", `String "The file to edit, e.g. \"notes.txt\"");
              ] );
          ( "target",
            `Assoc
              [
                ("type", `String "string");
                ("description", `String "The string to find");
              ] );
          ( "replacement",
            `Assoc
              [
                ("type", `String "string");
                ("description", `String "The string to replace it with");
              ] );
        ];
    run =
      (fun args ->
        try
          let open Yojson.Safe.Util in
          let filename = args |> member "filename" |> to_string in
          let target = args |> member "target" |> to_string in
          let replacement = args |> member "replacement" |> to_string in

          let%lwt original_content =
            if Sys.file_exists filename then
              Lwt_io.with_file ~mode:Lwt_io.Input filename Lwt_io.read
            else Lwt.return ""
          in

          let updated =
            Str.global_replace (Str.regexp_string target) replacement
              original_content
          in

          let%lwt () =
            Lwt_io.with_file ~mode:Lwt_io.Output filename (fun oc ->
                Lwt_io.write oc updated)
          in

          Lwt.return
            ("Updated file: " ^ filename ^ " — replaced \"" ^ target
           ^ "\" with \"" ^ replacement ^ "\"")
        with
        | Yojson.Json_error msg | Yojson.Safe.Util.Type_error (msg, _) ->
            Lwt.return ("Invalid input: " ^ msg)
        | exn -> Lwt.return ("[edit failed] " ^ Printexc.to_string exn));
  }

let tools = [ time_tool; math_tool; ls_tool; read_tool; edit_tool ]
let find_tool name = List.find_opt (fun t -> t.name = name) tools

let tool_response_message tool_output =
  { role = `User; content = "[Tool output for LLM to process]: " ^ tool_output }

let tool_description tool =
  let schema_str = Yojson.Safe.pretty_to_string tool.schema in
  Printf.sprintf "- %s: %s\n  Arguments JSON: %s" tool.name tool.description
    schema_str

let to_json tool =
  `Assoc
    [
      ("type", `String "function");
      ( "function",
        `Assoc
          [
            ("name", `String tool.name);
            ("description", `String tool.description);
            ("schema", tool.schema);
          ] );
    ]

let all_to_json () = `List (List.map to_json tools)
