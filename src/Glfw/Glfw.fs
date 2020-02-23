namespace Glfw

open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open System.Threading
open Silk.NET.GLFW
open OpenTK.Graphics.OpenGL4

#nowarn "9"

module internal OpenTKContext = 
    open System.Runtime.InteropServices
    open System.Reflection
    open OpenTK
    open OpenTK.Graphics
    open OpenTK.Platform
    open OpenTK.Graphics.OpenGL4

    type MyWindowInfo(win : nativeptr<WindowHandle>) =
        interface OpenTK.Platform.IWindowInfo with
            member x.Dispose(): unit = 
                ()
            member x.Handle: nativeint = 
                NativePtr.toNativeInt win
    
    [<AllowNullLiteral>]
    type MyGraphicsContext(glfw : Glfw, win : nativeptr<WindowHandle>) as this =
        [<System.ThreadStaticAttribute; DefaultValue>]
        static val mutable private CurrentContext : OpenTK.ContextHandle

        static let addContext = typeof<GraphicsContext>.GetMethod("AddContext", BindingFlags.NonPublic ||| BindingFlags.Static)


        static do 
            let get = GraphicsContext.GetCurrentContextDelegate(fun () -> MyGraphicsContext.CurrentContext)
            let t = typeof<GraphicsContext>
            let f = t.GetField("GetCurrentContext", BindingFlags.NonPublic ||| BindingFlags.Static)
            f.SetValue(null, get)

        do addContext.Invoke(null, [| this :> obj |]) |> ignore

        member x.LoadAll(): unit = 
            let t = typeof<GL>
            let m = t.GetMethod("LoadEntryPoints", BindingFlags.NonPublic ||| BindingFlags.Instance)
            let gl = GL()
            m.Invoke(gl, null) |> ignore
        
        interface IGraphicsContext with
            member x.Dispose(): unit = 
                ()

            member x.ErrorChecking
                with get () = false
                and set _ = ()

            member x.GraphicsMode = 
                GraphicsMode.Default

            member x.IsCurrent =
                glfw.GetCurrentContext() = win
            member x.IsDisposed: bool = 
                false
            member x.LoadAll() = x.LoadAll()
            member x.MakeCurrent(window: IWindowInfo): unit = 
                if isNull window then 
                    glfw.MakeContextCurrent(NativePtr.zero)
                    MyGraphicsContext.CurrentContext <- ContextHandle.Zero
                else 
                    MyGraphicsContext.CurrentContext <- ContextHandle(NativePtr.toNativeInt win)
                    glfw.MakeContextCurrent(win)

            member x.SwapBuffers(): unit = 
                glfw.SwapBuffers(win)
            member x.SwapInterval
                with get() = 0
                and set v = ()
            member x.Update(window: IWindowInfo): unit = 
                ()

        interface IGraphicsContextInternal with
            member x.Context: ContextHandle = 
                ContextHandle(NativePtr.toNativeInt win)
            member x.GetAddress(name : string): nativeint = 
                glfw.GetProcAddress name
            member x.GetAddress(name: nativeint): nativeint = 
                let str = Marshal.PtrToStringAnsi name
                glfw.GetProcAddress str
            member x.Implementation: IGraphicsContext = 
                x :> _
            member x.LoadAll() = x.LoadAll()
        

type internal WindowEvent =
    | Resize of width : int * height : int
    | Run of action : (unit -> unit)


type KeyEvent(key : Keys, scanCode : int, action : InputAction, modifiers : KeyModifiers, keyName : string) =
    member x.Key = key
    member x.ScanCode = scanCode
    member x.Actin = action
    member x.Modifiers = modifiers    
    member x.Name = keyName    

type Window internal(dummy : nativeptr<WindowHandle>, glfw : Glfw, win : nativeptr<WindowHandle>, ctx : OpenTK.Graphics.IGraphicsContext, info : OpenTK.Platform.IWindowInfo) =
    let mutable visible = false
    let mutable closing = false
    let eventLock = obj()
    let mutable events = System.Collections.Generic.List<WindowEvent>()
    let mutable mainThread : Thread = null
    let mutable size = V2i.II

    let mutable icon : option<PixImageMipMap> = None

    static let keyNameCache = System.Collections.Concurrent.ConcurrentDictionary<Keys * int, string>()

    let getKeyName(key : Keys) (code : int) =
        keyNameCache.GetOrAdd((key, code), fun (key, code) ->
            let c = if code >= 0 then code else glfw.GetKeyScancode(int key)
            let str = glfw.GetKeyName(int key, c)
            if System.String.IsNullOrWhiteSpace str then
                string key
            else
                let a = str.Substring(0, 1)
                let b = str.Substring(1)   
                a.ToUpper() + b
        )

    let keyDown = Event<KeyEvent>()
    let keyUp = Event<KeyEvent>()
    let resize = Event<V2i>()
    let focus = Event<bool>()


    let keyCallback =
        glfw.SetKeyCallback(win, GlfwCallbacks.KeyCallback(fun w k c a m ->
            let name = getKeyName k c
            match a with
            | InputAction.Press -> keyDown.Trigger(KeyEvent(k, c, a, m, name))
            | InputAction.Repeat -> keyDown.Trigger(KeyEvent(k,c, a, m, name))
            | InputAction.Release -> keyUp.Trigger(KeyEvent(k, c, a, m, name))
            | _ -> ()
        ))

    let closingCallback = 
        glfw.SetWindowCloseCallback(win, GlfwCallbacks.WindowCloseCallback(fun w ->
            closing <- true
        ))

    let focusCallback =
        glfw.SetWindowFocusCallback(win, GlfwCallbacks.WindowFocusCallback(fun w f ->
            focus.Trigger(f)
        ))


    member x.Context = ctx
    member x.WindowInfo = info

    [<CLIEvent>]
    member x.KeyDown = keyDown.Publish
    [<CLIEvent>]
    member x.KeyUp = keyUp.Publish
    [<CLIEvent>]
    member x.Resize = resize.Publish
    [<CLIEvent>]
    member x.FocusChanged = focus.Publish

    member x.GetKeyName(key : Keys, code : int) =
        match keyNameCache.TryGetValue((key, code)) with
        | (true, name) -> name
        | _ -> x.Invoke(fun () -> getKeyName key code)

    member x.GetKeyName(key : Keys) = x.GetKeyName(key, -1)
        

    member private x.RunEvents(evts : seq<WindowEvent>) =
        for e in evts do
            match e with
            | Run action -> action()
            | Resize(w,h) -> resize.Trigger(V2i(w,h))

    member internal x.Post(msg : WindowEvent) =
        lock eventLock (fun () ->
            events.Add msg
        )
        glfw.PostEmptyEvent()

    member x.Invoke<'r>(action : unit -> 'r) : 'r =
        let t = Thread.CurrentThread
        if t = mainThread || isNull mainThread then
            mainThread <- t
            action()
        else            
            let m = obj()
            let mutable result : option<Result<'r, exn>> = None
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


    member x.IsVisible 
        with get() = visible
        and set v =
            if v <> visible then
                x.Invoke (fun () ->
                    visible <- v
                    if v then glfw.ShowWindow(win)
                    else glfw.HideWindow(win)
                )

    member x.Focused
        with get() =
            x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.Focused))
        and set (f : bool) =
            x.Invoke(fun () ->
                let c = glfw.GetWindowAttrib(win, WindowAttributeGetter.Focused)
                if c <> f then 
                    if f then glfw.FocusWindow(win)
                    else glfw.FocusWindow(dummy)
            )                

    member x.Size
        with get() = 
            x.Invoke(fun () ->
                let (w,h) = glfw.GetWindowSize(win)
                V2i(w,h)
            )
        and set (size : V2i) =
            x.Invoke(fun () ->
                glfw.SetWindowSize(win, size.X, size.Y)
            )        

    member x.Icon 
        with get() = icon
        and set (v : option<PixImageMipMap>) =
            x.Invoke(fun () ->
                let inline toMatrix(img : PixImage<byte>) =
                    let dst = Matrix<uint32>(img.Size)
                    dst.SetMap(img.GetMatrix<C4b>(), fun c ->
                        (uint32 c.A <<< 24) ||| (uint32 c.B <<< 16) ||| (uint32 c.G <<< 8) ||| (uint32 c.R)
                    ) |> ignore
                    dst

                let rec mipMaps (acc : System.Collections.Generic.List<Image>) (i : int) (img : PixImage[]) =
                    if i >= img.Length then
                        let arr = acc.ToArray()
                        use img = fixed arr
                        glfw.SetWindowIcon(win, acc.Count, img)
                    else
                        let mat = toMatrix (img.[i].ToPixImage<byte>())
                        use pdata = fixed mat.Data
                        acc.Add(Image(Width = int mat.Size.X, Height = int mat.Size.Y, Pixels = NativePtr.cast pdata))

                        mipMaps acc (i+1) img                       

                match v with
                | Some img ->
                    let l = System.Collections.Generic.List()
                    mipMaps l 0 img.ImageArray
                | None ->
                    glfw.SetWindowIcon(win, 0, NativePtr.zero)    
                icon <- v                            
            )

    member x.Close() =
        x.Invoke(fun () ->
            glfw.HideWindow(win)
            closing <- true
        )
        


    member x.Run() =
        mainThread <- Thread.CurrentThread
        closing <- false
        x.IsVisible <- true
        let mutable frameCounter = 0
        let sw = System.Diagnostics.Stopwatch.StartNew()

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
                let (w,h)= glfw.GetWindowSize(win)
                let s = V2i(w,h)                
                if s <> size then 
                    size <- s
                    Seq.append (Seq.singleton (Resize(w, h))) myEvents
                else 
                    myEvents


            x.RunEvents myEvents
            if not (isNull ctx) then
                GL.ClearColor(1.0f, 0.0f, 0.0f, 1.0f)
                GL.Clear(ClearBufferMask.ColorBufferBit)

                glfw.SwapBuffers(win)   

            if frameCounter >= 100 then
                sw.Stop()
                let str = sprintf "%.2ffps" (float frameCounter / sw.Elapsed.TotalSeconds)
                glfw.SetWindowTitle(win, str)
                frameCounter <- 0
                sw.Restart()                


            frameCounter <- frameCounter + 1                     
            





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
    let mutable private lastWindow = None
   
    let private glfw = Glfw.GetApi()
    do if not (glfw.Init()) then
        failwith "GLFW init failed"

    let private dummyWindow =
        glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.NoApi)
        glfw.WindowHint(WindowHintBool.Visible, false)
        let win = glfw.CreateWindow(32, 32, "", NativePtr.zero, NativePtr.zero)
        win

    let create (cfg : WindowConfig) =
        lock l (fun () ->
            let mutable glContext = false
            let mutable parent : nativeptr<WindowHandle> = NativePtr.zero
            glfw.DefaultWindowHints()

            match cfg.opengl with
            | Some (major, minor) ->
                glContext <- true
                glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.OpenGL)
                glfw.WindowHint(WindowHintInt.ContextVersionMajor, major)
                glfw.WindowHint(WindowHintInt.ContextVersionMinor, minor)
                glfw.WindowHint(WindowHintInt.DepthBits, 24)
                glfw.WindowHint(WindowHintInt.StencilBits, 8)
                glfw.WindowHint(WindowHintInt.RedBits, 8)
                glfw.WindowHint(WindowHintInt.GreenBits, 8)
                glfw.WindowHint(WindowHintInt.BlueBits, 8)
                glfw.WindowHint(WindowHintInt.AlphaBits, 8)
                glfw.WindowHint(WindowHintOpenGlProfile.OpenGlProfile, OpenGlProfile.Core)
                glfw.WindowHint(WindowHintRobustness.ContextRobustness, Robustness.LoseContextOnReset)
                glfw.WindowHint(WindowHintBool.OpenGLForwardCompat, true)
                glfw.WindowHint(WindowHintBool.DoubleBuffer, true)
                glfw.WindowHint(WindowHintBool.OpenGLDebugContext, false)
                glfw.WindowHint(WindowHintBool.ContextNoError, true)
                match lastWindow with
                | Some l -> parent <- l
                | None -> ()
            | None ->
                glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.NoApi)

            glfw.WindowHint(WindowHintBool.Visible, false)
            glfw.WindowHint(WindowHintBool.Resizable, cfg.resizable)
            glfw.WindowHint(WindowHintInt.RefreshRate, cfg.refreshRate)
            glfw.WindowHint(WindowHintBool.FocusOnShow, cfg.focus)
            let win = glfw.CreateWindow(cfg.width, cfg.height, cfg.title, NativePtr.zero, parent)
            if win = NativePtr.zero then failwith "GLFW could not create window"


            if glContext then lastWindow <- Some win

            let ctx =
                if glContext then new OpenTKContext.MyGraphicsContext(glfw, win) :> OpenTK.Graphics.IGraphicsContext        
                else null

            let info =
                new OpenTKContext.MyWindowInfo(win)

            if not (isNull ctx) then  
                ctx.MakeCurrent info
                ctx.LoadAll()          

            glfw.SwapInterval(0)
            Window(dummyWindow, glfw, win, ctx, info)
        )
