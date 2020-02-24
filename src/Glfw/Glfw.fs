namespace Glfw

open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open System.Threading
open Silk.NET.GLFW
open OpenTK.Graphics.OpenGL4
open System.Runtime.InteropServices

#nowarn "9"

[<AutoOpen>]
module MissingGlfwFunctions =
    open System.Runtime.InteropServices

    type private GetWindowContentScaleDel = delegate of nativeptr<WindowHandle> * byref<float32> * byref<float32> -> unit

    let private dict = System.Collections.Concurrent.ConcurrentDictionary<Glfw, GetWindowContentScaleDel>()

    let private getWindowScale (glfw : Glfw) =
        dict.GetOrAdd(glfw, fun glfw ->
            let m = glfw.Library.LoadFunction "glfwGetWindowContentScale"
            Marshal.GetDelegateForFunctionPointer(m, typeof<GetWindowContentScaleDel>) |> unbox<GetWindowContentScaleDel>
        )

    type Glfw with
        member x.GetWindowContentScale(win : nativeptr<WindowHandle>, [<Out>] sx : byref<float32>, [<Out>] sy : byref<float32>) =
            getWindowScale(x).Invoke(win, &sx, &sy)


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
    | Resize
    | Run of action : (unit -> unit)


type KeyEvent(key : Keys, scanCode : int, action : InputAction, modifiers : KeyModifiers, keyName : string) =
    member x.Key = key
    member x.ScanCode = scanCode
    member x.Action = action
    member x.Modifiers = modifiers    
    member x.Name = keyName  

    member x.IsRepeat = action = InputAction.Repeat
    member x.Alt = int (modifiers &&& KeyModifiers.Alt) <> 0
    member x.Shift = int (modifiers &&& KeyModifiers.Shift) <> 0
    member x.Ctrl = int (modifiers &&& KeyModifiers.Control) <> 0
    member x.Super = int (modifiers &&& KeyModifiers.Super) <> 0

    override x.ToString() =
        let kind =
            match action with
            | InputAction.Press -> "KeyDown"
            | InputAction.Repeat -> "KeyRepeat"
            | InputAction.Release -> "KeyUp"
            | _ -> "KeyUnknown"

        let modifiers = 
            [
                if x.Alt then "alt"
                if x.Shift then "shift"
                if x.Ctrl then "ctrl"
                if x.Super then "super"          
            ]

        sprintf "%s { value: %A; scan: %A; mod: [%s]; name: %s }" kind key scanCode (String.concat "; " modifiers) keyName

type ResizeEvent(framebufferSize : V2i, physicalSize : V2i, windowSize : V2i) =
    member x.FramebufferSize = framebufferSize
    member x.PhysicalSize = physicalSize
    member x.WindowSize = windowSize

    override x.ToString() = 
        sprintf "Resize { framebuffer: %A; physical: %A; window: %A }" framebufferSize physicalSize windowSize


type MouseEvent(button : MouseButton, position: V2d, action : InputAction, modifiers : KeyModifiers) =
    member x.Button = button
    member x.Action = action
    member x.Modifiers = modifiers  
    member x.IsRepeat = action = InputAction.Repeat
    member x.Alt = int (modifiers &&& KeyModifiers.Alt) <> 0
    member x.Shift = int (modifiers &&& KeyModifiers.Shift) <> 0
    member x.Ctrl = int (modifiers &&& KeyModifiers.Control) <> 0
    member x.Super = int (modifiers &&& KeyModifiers.Super) <> 0

    override x.ToString() =
        let kind =
            match action with
            | InputAction.Press -> "MouseDown"
            | InputAction.Release -> "MouseUp"
            | _ -> "MouseUnknown"

        let modifiers = 
            [
                if x.Alt then "alt"
                if x.Shift then "shift"
                if x.Ctrl then "ctrl"
                if x.Super then "super"          
            ]

        sprintf "%s { value: %A; mod: [%s]; position: %A }" kind button (String.concat "; " modifiers) position

type WindowState =
    | Normal = 0
    | Minimized = 1
    | Maximized = 2
    | Invisible = 3


type Window internal(glfw : Glfw, win : nativeptr<WindowHandle>, title : string, ctx : OpenTK.Graphics.IGraphicsContext, info : OpenTK.Platform.IWindowInfo) =
    let mutable closing = false
    let eventLock = obj()
    let mutable events = System.Collections.Generic.List<WindowEvent>()
    let mutable mainThread : Thread = null
    let mutable framebufferSize = V2i.II
    let mutable windowScale = V2d.II
    let mutable damaged = true
    let mutable title = title
    let mutable icon : option<PixImageMipMap> = None
    let mutable lastMousePosition = V2d.Zero

    let getWindowState() =
        let vis = glfw.GetWindowAttrib(win, WindowAttributeGetter.Visible)
        if vis then 
            let min = glfw.GetWindowAttrib(win, WindowAttributeGetter.Iconified)
            let max = glfw.GetWindowAttrib(win, WindowAttributeGetter.Maximized)
            if min then WindowState.Minimized
            elif max then WindowState.Maximized
            else WindowState.Normal
        else
            WindowState.Invisible        

    let mutable windowState = getWindowState()

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
    let closeEvt = Event<unit>()    
    let resize = Event<ResizeEvent>()
    let wpChanged = Event<V2i>()
    let cpChanged = Event<V2i>()
    let focus = Event<bool>()
    let stateChanged = Event<WindowState>()
    let dropFiles = Event<string[]>()

    let keyDown = Event<KeyEvent>()
    let keyUp = Event<KeyEvent>()
    let keyInput = Event<string>()

    let mouseMove = Event<V2d>()
    let mouseDown = Event<MouseEvent>()
    let mouseUp = Event<MouseEvent>()
    let mouseWheel = Event<V2d>()
    let mouseEnter = Event<V2d>()
    let mouseLeave = Event<V2d>()
    



    let getFrameBorder() =
        if glfw.GetWindowAttrib(win, WindowAttributeGetter.Decorated) then
            let mutable border = Border2i()
            glfw.GetWindowFrameSize(win, &border.Min.X, &border.Min.Y, &border.Max.X, &border.Max.Y)
            if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) && System.Environment.OSVersion.Version.Major = 10 then
                if glfw.GetWindowAttrib(win, WindowAttributeGetter.Decorated) then
                    // see https://github.com/glfw/glfw/issues/539
                    border.Min.X <- 1
                    border.Max.X <- 1
                    border.Max.Y <- 1
            border  
        else
            Border2i()              

    let getResizeEvent() =
        let mutable fbo = V2i.Zero
        let mutable ps = V2i.Zero
        let mutable scale = V2f.Zero
        let border = getFrameBorder()

        glfw.GetFramebufferSize(win, &fbo.X, &fbo.Y)
        glfw.GetWindowSize(win, &ps.X, &ps.Y)
        glfw.GetWindowContentScale(win, &scale.X, &scale.Y)   
        windowScale <- V2d scale
        let ws = ps + border.Min + border.Max
        let ps = V2i(round (float scale.X * float ps.X), round(float scale.Y * float ps.Y))
        let ws = V2i(round (float scale.X * float ws.X), round(float scale.Y * float ws.Y))        

        ResizeEvent(
            fbo,
            ps, 
            ws
        )

    let getMousePosition() =
        let v = 
            glfw.GetWindowAttrib(win, WindowAttributeGetter.Visible) &&
            not (glfw.GetWindowAttrib(win, WindowAttributeGetter.Iconified))
        if v then
            let mutable pos = V2d.Zero
            glfw.GetCursorPos(win, &pos.X, &pos.Y)
            lastMousePosition <- pos * windowScale

        lastMousePosition

    let mutable mouseInside = getMousePosition() 

    let maxCb =
        glfw.SetWindowMaximizeCallback(win, GlfwCallbacks.WindowMaximizeCallback(fun w b ->
            windowState <- getWindowState()
            stateChanged.Trigger windowState
        ))

    let minCb =
        glfw.SetWindowIconifyCallback(win, GlfwCallbacks.WindowIconifyCallback(fun w b ->
            windowState <- getWindowState()
            stateChanged.Trigger windowState
        ))

    let closingCallback = 
        glfw.SetWindowCloseCallback(win, GlfwCallbacks.WindowCloseCallback(fun w ->
            closeEvt.Trigger()
            closing <- true
        ))

    let focusCallback =
        glfw.SetWindowFocusCallback(win, GlfwCallbacks.WindowFocusCallback(fun w f ->
            focus.Trigger(f)
        ))

    let posCb =
        glfw.SetWindowPosCallback(win, GlfwCallbacks.WindowPosCallback(fun w x y ->
            let border = getFrameBorder()
            wpChanged.Trigger(V2i(x,y) - border.Min)
            cpChanged.Trigger(V2i(x,y))
        ))

    let keyCallback =
        glfw.SetKeyCallback(win, GlfwCallbacks.KeyCallback(fun w k c a m ->
            let name = getKeyName k c
            match a with
            | InputAction.Press -> keyDown.Trigger(KeyEvent(k, c, a, m, name))
            | InputAction.Repeat -> keyDown.Trigger(KeyEvent(k,c, a, m, name))
            | InputAction.Release -> keyUp.Trigger(KeyEvent(k, c, a, m, name))
            | _ -> ()
        ))

    let inputCallback =
        glfw.SetCharCallback(win, GlfwCallbacks.CharCallback (fun w c ->
            let str = System.Text.Encoding.UTF32.GetString(System.BitConverter.GetBytes(c))
            keyInput.Trigger(str)
        ))


    let moveCallback =
        glfw.SetCursorPosCallback(win, GlfwCallbacks.CursorPosCallback(fun w a b ->
            let v = 
                glfw.GetWindowAttrib(w, WindowAttributeGetter.Visible) &&
                not (glfw.GetWindowAttrib(w, WindowAttributeGetter.Iconified))
            if v then
                let p = windowScale * V2d(a,b)
                lastMousePosition <- p
                mouseMove.Trigger(p)
        ))

    let mouseCallback =
        glfw.SetMouseButtonCallback(win, GlfwCallbacks.MouseButtonCallback(fun w button action modifiers ->
            let mutable pos = V2d.Zero
            glfw.GetCursorPos(win, &pos.X, &pos.Y)
            let pos = windowScale * pos
            let evt = MouseEvent(button, pos, action, modifiers)
            match action with
            | InputAction.Press -> mouseDown.Trigger evt    
            | InputAction.Release -> mouseUp.Trigger evt
            | _ -> ()       
        ))    

    let wheelCallback =
        glfw.SetScrollCallback(win, GlfwCallbacks.ScrollCallback(fun w dx dy ->
            mouseWheel.Trigger(V2d(dx, dy))
        ))    

    let damagedCallback =
        glfw.SetWindowRefreshCallback(win, GlfwCallbacks.WindowRefreshCallback(fun w ->
            damaged <- true
        ))

    let dropCallback =
        glfw.SetDropCallback(win, GlfwCallbacks.DropCallback(fun _ cnt paths ->
            let ptr = NativePtr.ofNativeInt paths
            let files = 
                Array.init cnt (fun i ->
                    Marshal.PtrToStringUTF8 (NativePtr.get ptr i)
                )
            dropFiles.Trigger files                 
        ))

    let enterLeave =
        glfw.SetCursorEnterCallback(win, GlfwCallbacks.CursorEnterCallback(fun _ entered ->
            if entered then mouseEnter.Trigger(getMousePosition())
            else mouseLeave.Trigger(getMousePosition())
        ))    

    member x.Context = ctx
    member x.WindowInfo = info

    [<CLIEvent>]
    member x.WindowStateChanged = stateChanged.Publish
    [<CLIEvent>]
    member x.Closing = closeEvt.Publish
    [<CLIEvent>]
    member x.Resize = resize.Publish
    [<CLIEvent>]
    member x.FocusChanged = focus.Publish
    [<CLIEvent>]
    member x.WindowPositionChanged = wpChanged.Publish
    [<CLIEvent>]
    member x.ContentPositionChanged = cpChanged.Publish
    [<CLIEvent>]
    member x.DropFiles = dropFiles.Publish
    [<CLIEvent>]
    member x.KeyDown = keyDown.Publish
    [<CLIEvent>]
    member x.KeyUp = keyUp.Publish
    [<CLIEvent>]
    member x.KeyInput = keyInput.Publish

    [<CLIEvent>]
    member x.MouseDown = mouseDown.Publish
    [<CLIEvent>]
    member x.MouseUp = mouseUp.Publish
    [<CLIEvent>]
    member x.MouseMove = mouseMove.Publish
    [<CLIEvent>]
    member x.MouseWheel = mouseWheel.Publish
    [<CLIEvent>]
    member x.MouseEnter = mouseEnter.Publish
    [<CLIEvent>]
    member x.MouseLeave = mouseLeave.Publish


    member x.WindowState
        with get() = windowState
        and set (s : WindowState) =
            x.Invoke(fun () ->
                if s <> windowState then
                    windowState <- s
                    match s with
                    | WindowState.Maximized -> glfw.MaximizeWindow(win)
                    | WindowState.Minimized -> glfw.IconifyWindow(win)
                    | WindowState.Normal -> glfw.RestoreWindow(win)
                    | _ -> ()
            )   

    member x.Decorated
        with get() = x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.Decorated))
        and set b = x.Invoke(fun () -> glfw.SetWindowAttrib(win, WindowAttributeSetter.Decorated, b))         

    member x.Floating
        with get() = x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.Floating))
        and set b = x.Invoke(fun () -> glfw.SetWindowAttrib(win, WindowAttributeSetter.Floating, b))         

    member x.Resizable
        with get() = x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.Resizable))
        and set b = x.Invoke(fun () -> glfw.SetWindowAttrib(win, WindowAttributeSetter.Resizable, b))         

    member x.Transparent
        with get() = x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.TransparentFramebuffer))

    member x.MousePosition =
        x.Invoke(fun () ->
            let v = 
                glfw.GetWindowAttrib(win, WindowAttributeGetter.Visible) &&
                not (glfw.GetWindowAttrib(win, WindowAttributeGetter.Iconified))
            if v then
                let mutable pos = V2d.Zero
                glfw.GetCursorPos(win, &pos.X, &pos.Y)
                lastMousePosition <- pos * windowScale
            
            lastMousePosition         
        )

    member x.GetKeyName(key : Keys, code : int) =
        match keyNameCache.TryGetValue((key, code)) with
        | (true, name) -> name
        | _ -> x.Invoke(fun () -> getKeyName key code)

    member x.GetKeyName(key : Keys) = x.GetKeyName(key, -1)
        

    member private x.RunEvents(evts : seq<WindowEvent>) =
        for e in evts do
            match e with
            | Run action -> action()
            | Resize -> resize.Trigger(getResizeEvent())

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
        with get() =
            x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.Visible))         
        and set v =
            x.Invoke (fun () ->
                if v then glfw.ShowWindow(win)
                else  glfw.HideWindow(win)
            )

    member x.Title 
        with get() = title
        and set t =
            x.Invoke(fun () ->
                if title <> t then
                    title <- t
                    glfw.SetWindowTitle(win, t)
            ) 

    member x.Focus() = 
        x.Invoke(fun () ->
            let c = glfw.GetWindowAttrib(win, WindowAttributeGetter.Focused)
            if not c then glfw.FocusWindow(win)
        )       

    member x.Focused = x.Invoke(fun () -> glfw.GetWindowAttrib(win, WindowAttributeGetter.Focused))             

    member x.WindowSize
        with get() =
            x.Invoke(fun () ->
                let mutable ps = V2i.Zero
                let mutable scale = V2f.Zero
                let border = getFrameBorder()

                glfw.GetWindowSize(win, &ps.X, &ps.Y)
                glfw.GetWindowContentScale(win, &scale.X, &scale.Y)   
                let ws = ps + border.Min + border.Max
                V2i(round (float scale.X * float ws.X), round(float scale.Y * float ws.Y))  
            )
        and set (v : V2i) = 
            x.Invoke(fun () ->
                let mutable scale = V2f.Zero
                let border = getFrameBorder()

                glfw.GetWindowContentScale(win, &scale.X, &scale.Y)   

                let ws = V2i(round (float v.X / float scale.X), round(float v.Y / float scale.Y))  
                let ps = V2i.Max(V2i.II, ws - (border.Min + border.Max))

                glfw.SetWindowSize(win, ps.X, ps.Y)
            )

    member x.FramebufferSize
        with get() = 
            x.Invoke(fun () ->
                let (w,h) = glfw.GetFramebufferSize(win)
                V2i(w,h)
            )
        and set (v : V2i) =
            x.Invoke(fun () ->
                let mutable scale = V2f.Zero
                glfw.GetWindowContentScale(win, &scale.X, &scale.Y)   
                let ws = V2i(round (float v.X / float scale.X), round(float v.Y / float scale.Y)) 

                glfw.SetWindowSize(win, ws.X, ws.Y)
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

    member x.WindowPosition
        with get() = 
            x.Invoke(fun () ->
                let mutable pos = V2i.Zero
                glfw.GetWindowPos(win, &pos.X, &pos.Y)
                let b = getFrameBorder()
                pos - b.Min
            )
        and set (pos : V2i) =   
            x.Invoke(fun () ->
                let b = getFrameBorder()
                let pp = b.Min + pos
                glfw.SetWindowPos(win, pp.X, pp.Y)
            ) 

    member x.ContentPosition
        with get() = 
            x.Invoke(fun () ->
                let mutable pos = V2i.Zero
                glfw.GetWindowPos(win, &pos.X, &pos.Y)
                pos
            )
        and set (pos : V2i) =   
            x.Invoke(fun () ->
                glfw.SetWindowPos(win, pos.X, pos.Y)
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
                let mutable s = V2i.Zero            
                glfw.GetFramebufferSize(win, &s.X, &s.Y)       
                if s <> framebufferSize then 
                    framebufferSize <- s
                    Seq.append (Seq.singleton Resize) myEvents
                else 
                    myEvents


            x.RunEvents myEvents

            if damaged then
                damaged <- false
                if not (isNull ctx) then
                    GL.ClearColor(1.0f, 0.0f, 0.0f, 0.5f)
                    GL.Clear(ClearBufferMask.ColorBufferBit)

                    glfw.SwapBuffers(win)   

                if frameCounter >= 100 then
                    sw.Stop()
                    Log.line "%.2ffps" (float frameCounter / sw.Elapsed.TotalSeconds)
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
        transparent : bool
        opengl : option<int*int>
        physicalSize : bool
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
        glfw.WindowHint(WindowHintBool.Resizable, false)
        glfw.WindowHint(WindowHintBool.Decorated, false)
        glfw.WindowHint(WindowHintBool.TransparentFramebuffer, true)
        let win = glfw.CreateWindow(1, 1, "", NativePtr.zero, NativePtr.zero)
        glfw.SetWindowPos(win, 1000000000, 1000000000)
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
                if RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then
                    glfw.WindowHint(unbox<WindowHintBool> 0x00023001, cfg.physicalSize)

                glfw.WindowHint(unbox<WindowHintBool> 0x0002200C, false)
                match lastWindow with
                | Some l -> parent <- l
                | None -> ()
            | None ->
                glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.NoApi)

            glfw.WindowHint(WindowHintBool.TransparentFramebuffer, cfg.transparent)
            glfw.WindowHint(WindowHintBool.Visible, false)
            glfw.WindowHint(WindowHintBool.Resizable, cfg.resizable)
            glfw.WindowHint(WindowHintInt.RefreshRate, cfg.refreshRate)
            glfw.WindowHint(WindowHintBool.FocusOnShow, cfg.focus)


            let m = glfw.GetPrimaryMonitor()

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
            Window(glfw, win, cfg.title, ctx, info)
        )
