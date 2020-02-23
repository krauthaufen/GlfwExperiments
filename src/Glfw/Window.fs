namespace Bla

open Microsoft.FSharp.NativeInterop
open Aardvark.Base
open Silk.NET.Core
open Silk.NET.GLFW
open System.Runtime.InteropServices

#nowarn "9"

module DeviceChooser =

    module WindowsDeviceChooser =
        open System
        open System.Runtime.InteropServices
        open System.Runtime.CompilerServices

        [<Flags>]
        type DisplayDeviceFlags =
            | None = 0x00000000
            | Active = 0x00000001   
            | MultiDriver = 0x00000002 
            | PrimaryDevice = 0x00000004
            | MirroringDriver = 0x00000008
            | VgaCompatible = 0x00000010
            | Removable = 0x00000020
            | UnsafeModesOn = 0x00080000
            | Compatible = 0x00200000
            | Disconnect = 0x02000000
            | Remote = 0x04000000
            | ModesPruned = 0x08000000

        type DisplayDevice =
            {
                stateFlags      : DisplayDeviceFlags
                deviceName      : string      
                deviceKey       : string
                deviceId        : string
                deviceString    : string    
            }   

        module Kernel32 =

            type HDC = nativeint
            type HWND = nativeint
            type HINSTANCE = nativeint
            type HICON = nativeint
            type HCURSOR = nativeint
            type HBRUSH = nativeint
            type LPCSTR = nativeint
            type HMODULE = nativeint

            [<StructLayout(LayoutKind.Sequential)>]
            type WndClassA =
                struct
                    val mutable public style : uint32
                    val mutable public lpfnWndProc : nativeint
                    val mutable public cbClsExtra : int
                    val mutable public cbWndExtra : int
                    val mutable public hInstance : HINSTANCE
                    val mutable public hIcon : HICON
                    val mutable public hCursor : HCURSOR
                    val mutable public hBrush : HBRUSH
                    val mutable public lpszMenuName : LPCSTR
                    val mutable public lpszClassName : LPCSTR
                end


            [<DllImport("kernel32.dll")>]
            extern HMODULE GetModuleHandle(string lpModuleName)

            [<DllImport("kernel32.dll")>]
            extern HMODULE GetProcAddress(HMODULE m, string name)

            [<DllImport("kernel32.dll")>]
            extern void VirtualProtect(nativeint address, unativeint size, uint32 newProt, uint32& oldProt)

            [<DllImport("user32.dll")>]
            extern nativeint RegisterClass(WndClassA& clazz)

            [<DllImport("user32.dll")>]
            extern HWND CreateWindowEx(uint32 style, LPCSTR lpClassName, LPCSTR windowName, uint32 dwStyle, int x, int y, int w, int h, HWND parent, nativeint menu, nativeint p)

            [<DllImport("user32.dll")>]
            extern bool EnumDisplayDevicesA(string lpDevice, uint32 iDevNum, void* dev, uint32 dwFlags)


            let mutable hwnd : HWND = 0n

            let WindowFromDCReplacement(hdc : HDC) : HWND =
                if hdc = 0n then
                    System.Console.WriteLine("HDC null")
                    0n
                elif hwnd = 0n then
                    let mutable wc = Unchecked.defaultof<WndClassA>
                    wc.hInstance <- GetModuleHandle null
                    wc.lpszClassName <- Marshal.StringToHGlobalUni("_dummy_window_class_")
                    wc.lpfnWndProc <- GetProcAddress(GetModuleHandle "user32.dll", "DefWindowProcA")
                    RegisterClass(&wc) |> ignore

                    let wnd = 
                        CreateWindowEx(
                            0u,
                            wc.lpszClassName,
                            0n,
                            0x8000000u,
                            0, 0, 32, 32,
                            0n, 0n, 0n
                        )
                    System.Console.WriteLine("HWND: {0}", wnd)  
                    hwnd <- wnd
                    wnd
                else
                    hwnd                

            type WindowFromDCDelegate = delegate of HDC -> HWND
            let WindowFromDCD = WindowFromDCDelegate WindowFromDCReplacement
            let pWindowFromDC = Marshal.PinDelegate(WindowFromDCD)

        let readDevices () =
            let mem = Marshal.AllocHGlobal(512)
            NativeInt.write mem 512

            let arr = System.Collections.Generic.List<DisplayDevice>()
            let mutable di = 0

            let inline read di =
                Marshal.Set(mem, 0, 512)
                NativeInt.write mem 512
                Kernel32.EnumDisplayDevicesA(null, uint32 di, mem, 0u)

            while read di do

                let devName = Marshal.PtrToStringAnsi(mem + 4n, 32)
                let devStr = Marshal.PtrToStringAnsi(mem + 36n, 128)
                let stateFlags = NativeInt.read<int> (mem + 164n)
                let devId = Marshal.PtrToStringAnsi(mem + 168n, 128)
                let devKey = Marshal.PtrToStringAnsi(mem + 296n, 128)

                arr.Add {
                    stateFlags      = unbox<DisplayDeviceFlags> stateFlags
                    deviceName      = devName.Trim [|' '; '\n'; '\r'; char 0 |]
                    deviceKey       = devKey.Trim [|' '; '\n'; '\r'; char 0 |]
                    deviceId        = devId.Trim [|' '; '\n'; '\r'; char 0 |]
                    deviceString    = devStr.Trim [|' '; '\n'; '\r'; char 0 |]
                }   

                di <- di + 1

            Seq.toList arr

        let patchWindowFromDC() =
            NativeLibrary.TryLoad "nvapi64.dll" |> ignore

            let m = Kernel32.GetModuleHandle "user32.dll"
            if m <> 0n then
                let ptr = Kernel32.GetProcAddress(m, "WindowFromDC")
                if ptr <> 0n then
                    let old : byte[] = Array.zeroCreate 14
                    Marshal.Copy(ptr, old, 0, old.Length)

                    let mutable oldProt = 0u
                    Kernel32.VirtualProtect(ptr, 14un, 0x80u, &oldProt)

                    NativeInt.write ptr 0xFFuy
                    NativeInt.write (ptr + 1n) 0x25uy
                    NativeInt.write (ptr + 2n) 0x00000000
                    NativeInt.write (ptr + 6n) (int64 Kernel32.pWindowFromDC.Pointer)

                    { new System.IDisposable with
                        member x.Dispose() =
                            Marshal.Copy(old, 0, ptr, old.Length)
                    }
                else
                    failwith "could not patch WindowFromDC" 
            else
                failwith "could not patch WindowFromDC"                



        let run () =
            let patch = patchWindowFromDC()


            for d in readDevices() do   
                if d.deviceKey.EndsWith "00" then
                    Log.start "%s" d.deviceName
                    Log.line "str: %s" d.deviceString
                    Log.line "key: %s" d.deviceKey
                    Log.line "id:  %s" d.deviceId
                    Log.line "flg: %A" d.stateFlags
                    Log.stop()



            // HDC dc = CreateDCA("\\\\.\\DISPLAY4", "\\\\.\\DISPLAY4", NULL, NULL);
            // memset(&pfd, 0, sizeof(pfd));
            // pfd.nSize = sizeof(pfd);
            // pfd.nVersion = 1;
            // pfd.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER_DONTCARE | PFD_STEREO_DONTCARE | PFD_DEPTH_DONTCARE;
            // pfd.iPixelType = PFD_TYPE_RGBA;
            // pfi = ChoosePixelFormat(dc, &pfd);
            // SetPixelFormat(dc, pfi, &pfd);
            // glrc = wglCreateContext(dc);
            // wglMakeCurrent(dc, glrc);


            let glfw = Glfw.GetApi()

            glfw.Init() |> ignore

            let mutable cnt = 0
            let monitors = glfw.GetMonitors(&cnt)

            for c in 0 .. cnt - 1 do
                let m = NativePtr.get monitors c |> NativePtr.read
                Log.warn "%A" m

            glfw.WindowHint(WindowHintInt.ContextVersionMajor, 4)
            glfw.WindowHint(WindowHintInt.ContextVersionMinor, 5)
            glfw.WindowHint(WindowHintBool.Visible, false)
            glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.OpenGL)
            glfw.WindowHint(WindowHintBool.Resizable, false)
            glfw.WindowHint(WindowHintInt.RefreshRate, 0)
            glfw.WindowHint(WindowHintBool.FocusOnShow, true)
            let win = glfw.CreateWindow(1024, 768, "Bla", NativePtr.zero, NativePtr.zero)


            glfw.ShowWindow win
            let mutable closing = false
            glfw.SetWindowCloseCallback(win, GlfwCallbacks.WindowCloseCallback(fun win -> closing <- true)) |> ignore
            while not closing do
                glfw.WaitEvents()

            patch.Dispose()

            ()


    let run() =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then WindowsDeviceChooser.run()
        else ()

module Window =
    open Silk.NET.OpenGL

    let private check fmt =
        fmt |> Printf.kprintf (fun str ->
            fun v ->
                if not v then Report.Warn("{0}", str)
        )

    type MyLoader(glfw : Glfw) =
        inherit Silk.NET.Core.Loader.GLSymbolLoader()
        override x.CoreLoadFunctionPointer(_, name : string) =
            glfw.GetProcAddress name

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

        static let addContext = typeof<OpenTK.Graphics.GraphicsContext>.GetMethod("AddContext", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static)


        static do 
            let get = OpenTK.Graphics.GraphicsContext.GetCurrentContextDelegate(fun () -> MyGraphicsContext.CurrentContext)
            let t = typeof<OpenTK.Graphics.GraphicsContext>
            let f = t.GetField("GetCurrentContext", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Static)
            f.SetValue(null, get)

        do addContext.Invoke(null, [| this :> obj |]) |> ignore

        member x.LoadAll(): unit = 
            let t = typeof<OpenTK.Graphics.OpenGL4.GL>
            let m = t.GetMethod("LoadEntryPoints", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
            let gl = OpenTK.Graphics.OpenGL4.GL()
            m.Invoke(gl, null) |> ignore

            let t = typeof<OpenTK.Graphics.OpenGL.GL>
            let m = t.GetMethod("LoadEntryPoints", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)
            let gl = OpenTK.Graphics.OpenGL.GL()
            m.Invoke(gl, null) |> ignore
        
        interface OpenTK.Graphics.IGraphicsContext with
            member x.Dispose(): unit = 
                ()

            member x.ErrorChecking
                with get () = false
                and set _ = ()

            member x.GraphicsMode = 
                OpenTK.Graphics.GraphicsMode.Default

            member x.IsCurrent =
                glfw.GetCurrentContext() = win
            member x.IsDisposed: bool = 
                false
            member x.LoadAll() = x.LoadAll()
            member x.MakeCurrent(window: OpenTK.Platform.IWindowInfo): unit = 
                if isNull window then 
                    glfw.MakeContextCurrent(NativePtr.zero)
                    MyGraphicsContext.CurrentContext <- OpenTK.ContextHandle.Zero
                else 
                    MyGraphicsContext.CurrentContext <- OpenTK.ContextHandle(NativePtr.toNativeInt win)
                    glfw.MakeContextCurrent(win)

            member x.SwapBuffers(): unit = 
                glfw.SwapBuffers(win)
            member x.SwapInterval
                with get() = 0
                and set v = ()
            member x.Update(window: OpenTK.Platform.IWindowInfo): unit = 
                ()

        interface OpenTK.Graphics.IGraphicsContextInternal with
            member x.Context: OpenTK.ContextHandle = 
                OpenTK.ContextHandle(NativePtr.toNativeInt win)
            member x.GetAddress(name : string): nativeint = 
                glfw.GetProcAddress name
            member x.GetAddress(name: nativeint): nativeint = 
                let str = Marshal.PtrToStringAnsi name
                glfw.GetProcAddress str
            member x.Implementation: OpenTK.Graphics.IGraphicsContext = 
                x :> _
            member x.LoadAll() = x.LoadAll()
        


    let create() =

        let glfw = Glfw.GetApi()
        glfw.Init() |> check "glfwInit"


        glfw.WindowHint(WindowHintInt.ContextVersionMajor, 4)
        glfw.WindowHint(WindowHintInt.ContextVersionMinor, 5)
        glfw.WindowHint(WindowHintBool.Visible, false)
        glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.OpenGL)
        glfw.WindowHint(WindowHintBool.Resizable, false)
        glfw.WindowHint(WindowHintInt.RefreshRate, 0)
        glfw.WindowHint(WindowHintBool.FocusOnShow, true)
        let win = glfw.CreateWindow(1024, 768, "Bla", NativePtr.zero, NativePtr.zero)


        let ctx = new MyGraphicsContext(glfw, win) :> OpenTK.Graphics.IGraphicsContext
        let wi = new MyWindowInfo(win) :> OpenTK.Platform.IWindowInfo
        Log.warn "%A" ctx.IsCurrent



        ctx.MakeCurrent wi 
        
        Log.warn "%A" ctx.IsCurrent

        ctx.LoadAll()   
        Log.warn "%A" <| OpenTK.Graphics.OpenGL4.GL.GetString(OpenTK.Graphics.OpenGL4.StringName.Renderer)

        ctx.MakeCurrent null 
        Log.warn "%A" ctx.IsCurrent

        System.Environment.Exit 0

        glfw.MakeContextCurrent(win)
        Silk.NET.Core.Platform.SilkManager.Register(MyLoader glfw :> Silk.NET.Core.Loader.GLSymbolLoader)
        let gl = Silk.NET.OpenGL.GL.GetApi()

        let major = gl.GetInteger(GLEnum.MajorVersion)
        let minor = gl.GetInteger(GLEnum.MinorVersion)

        let vendor = gl.GetString(StringName.Vendor)
        let renderer = gl.GetString(StringName.Renderer)
        let version = gl.GetString(StringName.Version)
        

        //glfw.MakeContextCurrent(NativePtr.zero)
        Log.warn "%s" vendor
        Log.warn "%s" renderer
        Log.warn "%s" version
        Log.warn "%d.%d" major minor



        glfw.SetKeyCallback(win, GlfwCallbacks.KeyCallback(fun win key code action mods -> 
            match action with
            | InputAction.Press ->
                let name = glfw.GetKeyName(int key, code)
                let displayName = 
                    if System.String.IsNullOrWhiteSpace name then string key
                    else name.ToUpper()
                Log.warn "%s" displayName
            | InputAction.Repeat ->
                let name = glfw.GetKeyName(int key, code)
                let displayName = 
                    if System.String.IsNullOrWhiteSpace name then string key
                    else name.ToUpper()
                Log.warn "repeat %s" displayName

            | _ ->
                ()            
        )) |> ignore

        let eventLock = obj()
        let mutable events = System.Collections.Generic.Queue<string>()                        
        let thread =
            startThread (fun () ->
                while true do
                    let l = System.Console.ReadLine()
                    lock eventLock (fun () ->
                        events.Enqueue "hi"
                    )
                    glfw.PostEmptyEvent()
            )                

        let mutable closing = false
        glfw.SetWindowCloseCallback(win, GlfwCallbacks.WindowCloseCallback(fun win -> closing <- true)) |> ignore
        glfw.ShowWindow(win)

        let setIcon() = 
            let w = 16
            let h = 16
            let data =
                let a = C4b.White
                let b = C4b.VRVisGreen
                let ai = (uint32 a.A <<< 24) ||| (uint32 a.B <<< 16) ||| (uint32 a.G <<< 8) ||| (uint32 a.R)
                let bi = (uint32 b.A <<< 24) ||| (uint32 b.B <<< 16) ||| (uint32 b.G <<< 8) ||| (uint32 b.R)
                Array.init (w*h) (fun i ->
                    let x = (i%w) / 4
                    let y = (i/w) / 4
                    if (x+y)%2 = 0 then ai
                    else bi             
                )

            use ptr = fixed data
            use image = fixed [| Image(Width = w, Height = h, Pixels = NativePtr.cast ptr) |]
            glfw.SetWindowIcon(win, 1, image)

        setIcon()

        while not closing do
            glfw.WaitEvents()
            let pending = 
                lock eventLock (fun () ->
                    let o = events
                    events <- System.Collections.Generic.Queue<_>()
                    o
                )   
            while pending.Count > 0 do
                let e = pending.Dequeue()
                Log.warn "%A" e                                     

            gl.ClearColor(1.0f, 0.0f, 0.0f, 1.0f)
            gl.Clear(uint32 ClearBufferMask.ColorBufferBit)
            glfw.SwapBuffers(win)











