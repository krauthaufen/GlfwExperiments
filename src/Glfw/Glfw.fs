namespace Glfw

open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open System.Threading
open Silk.NET.GLFW

type internal WindowEvent =
    | Redraw
    | Resize of width : int * height : int
    | Run of action : (unit -> unit)


type KeyEvent(key : Keys, scanCode : int, action : InputAction, modifiers : KeyModifiers) =
    member x.Key = key
    member x.ScanCode = scanCode
    member x.Actin = action
    member x.Modifiers = modifiers        

type Window internal(glfw : Glfw, win : nativeptr<WindowHandle>) =
    let mutable visible = false
    let mutable closing = false
    let eventLock = obj()
    let mutable events = System.Collections.Generic.List<WindowEvent>()
    let mutable mainThread : Thread = null
    let mutable size = (-1,-1)

    let keyDown = Event<KeyEvent>()
    let keyUp = Event<KeyEvent>()



    let keyCallback =
        glfw.SetKeyCallback(win, GlfwCallbacks.KeyCallback(fun w k c a m ->
            match a with
            | InputAction.Press -> keyDown.Trigger(KeyEvent(k, c, a, m))
            | InputAction.Release -> keyUp.Trigger(KeyEvent(k, c, a, m))
            | _ -> ()
        ))

    let closingCallback = 
        glfw.SetWindowCloseCallback(win, GlfwCallbacks.WindowCloseCallback(fun w ->
            closing <- true
        ))

    [<CLIEvent>]
    member x.KeyDown = keyDown.Publish
    [<CLIEvent>]
    member x.KeyUp = keyUp.Publish

    member private x.RunEvents(evts : seq<WindowEvent>) =
        for e in evts do
            match e with
            | Run action -> action()
            | _ -> ()

    member internal x.Post(msg : WindowEvent) =
        lock eventLock (fun () ->
            events.Add msg
        )
        glfw.PostEmptyEvent()

    member x.Invoke(action : unit -> 'r) =
        let t = Thread.CurrentThread
        if t = mainThread || isNull mainThread then
            mainThread <- t
            action()
        else            
            let m = obj()
            let mutable result = None
            let run() =
                let value = 
                    try action() |> Result.Ok
                    with e -> Result.Error e
                lock m (fun () ->
                    result <- Some value
                    Monitor.PulseAll m
                )
            x.Post(Run(run))
            lock m (fun () ->
                while Option.isNone result do
                    Monitor.Wait(m) |> ignore
            )

            match result.Value with
            | Result.Ok v -> v
            | Result.Error e -> raise e


    member x.Close() =
        x.Invoke(fun () ->
            glfw.HideWindow(win)
            closing <- true
        )
        

    member x.IsVisible 
        with get() = visible
        and set v =
            if v <> visible then
                x.Invoke (fun () ->
                    visible <- v
                    if v then glfw.ShowWindow(win)
                    else glfw.HideWindow(win)
                )

    member x.Run() =
        mainThread <- Thread.CurrentThread
        closing <- false
        x.IsVisible <- true
        while not closing do
            glfw.WaitEvents()
            let myEvents = 
                lock eventLock (fun () ->
                    let o = events
                    if o.Count > 0 then 
                        events <- System.Collections.Generic.List()
                        o :> seq<_>
                    else
                        Seq.empty
                ) 
            let myEvents =                 
                let ((w,h) as s) = glfw.GetWindowSize(win)
                if s <> size then 
                    size <- s
                    Seq.append (Seq.singleton (Resize(w, h))) myEvents
                else 
                    myEvents


            x.RunEvents myEvents


type WindowConfig =
    {
        title : string
        width : int
        height : int
        focus : bool
        resizable : bool
        refreshRate : int
        opengl : option<int*int>
    }    

module Window = 
    let private l = obj()
    let mutable private initialized = false
    let mutable private lastWindow = None

    let create (cfg : WindowConfig) =
        lock l (fun () ->
            let glfw = Glfw.GetApi()
            if not initialized then
                if not (glfw.Init()) then
                    failwith "GLFW init failed"
                initialized <- true                

            let mutable storeWin = false
            let mutable parent : nativeptr<WindowHandle> = NativePtr.zero
            match cfg.opengl with
            | Some (major, minor) ->
                glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.OpenGL)
                glfw.WindowHint(WindowHintInt.ContextVersionMajor, major)
                glfw.WindowHint(WindowHintInt.ContextVersionMinor, minor)
                match lastWindow with
                | Some l -> parent <- l
                | None -> storeWin <- true
            | None ->
                glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.NoApi)

            glfw.WindowHint(WindowHintBool.Visible, false)
            glfw.WindowHint(WindowHintBool.Resizable, cfg.resizable)
            glfw.WindowHint(WindowHintInt.RefreshRate, cfg.refreshRate)
            glfw.WindowHint(WindowHintBool.FocusOnShow, cfg.focus)
            let win = glfw.CreateWindow(cfg.width, cfg.height, cfg.title, NativePtr.zero, parent)
            if storeWin then lastWindow <- Some win


            Window(glfw, win)
        )
