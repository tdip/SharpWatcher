namespace Tdip.SharpWatcher

type FileSystem<'attributes> =
    Directory of string*'attributes*list<FileSystem<'attributes>>

module FileSystem =

    let rec fromPath
        (lsChildren: string -> list<string>)
        (getAttributes: string -> 'attributes)
        (path: string)
        : FileSystem<'attributes> =

        let attributes = getAttributes path
        let children = lsChildren path
        Directory(
            path,
            attributes,
            List.map (fromPath lsChildren getAttributes) children
        )