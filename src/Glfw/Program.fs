open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Base.Rendering
open Glfw
open Aardvark.Application

[<EntryPoint;STAThread>]
let main argv = 
    let app = Application()

    let win = 
        app.CreateWindow {
            title = "Dummy"            
            width = 1024
            height = 768
            resizable = true
            focus = true
            refreshRate = 0
            opengl = Some(4,1)
            physicalSize = false
            transparent = true
        }

    
    
    win.WindowPositionChanged.Add(fun e ->
        Log.warn "move: %A" e
    )

    win.MouseMove.Add (fun e ->
        win.Title <- sprintf "mouse: %s" (e.ToString("0.00"))
    )

    win.FocusChanged.Add(fun e ->
        Log.warn "focus: %A" e
    )

    win.Resize.Add (fun s ->
        Log.warn "resize: %A" s    
    )    

    win.KeyDown.Add(fun e ->

        let setPos (p : V2i) =
            let mutable p = p
            if p.X < 0 then p.X <- 0
            if p.Y < 0 then p.Y <- 0

            let s = win.WindowSize

            if p.X + s.X > 1920 then p.X <- 1920 - s.X
            if p.Y + s.Y > 1080 then p.Y <- 1080 - s.Y
            win.WindowPosition <- p

        if e.Key = Keys.Escape then win.Close()
        elif e.Key = Keys.Right then setPos (win.WindowPosition + V2i(20, 0))
        elif e.Key = Keys.Left then setPos (win.WindowPosition - V2i(20, 0))
        elif e.Key = Keys.Up then setPos (win.WindowPosition - V2i(0, 20))
        elif e.Key = Keys.Down then setPos (win.WindowPosition + V2i(0, 20))
        elif e.Key = Keys.R then win.WindowSize <- V2i(180, 180)
        elif e.Key = Keys.T then win.WindowSize <- V2i(1024, 768)
        elif e.Key = Keys.Q then win.FramebufferSize <- V2i(180, 180)
        elif e.Key = Keys.W then win.FramebufferSize <- V2i(1024, 768)
        elif e.Key = Keys.Back then
            match win.WindowState with
            | WindowState.Minimized -> win.WindowState <- WindowState.Normal
            | WindowState.Maximized -> win.WindowState <- WindowState.Normal
            | _ -> win.WindowState <- WindowState.Maximized
        elif e.Key = Keys.Delete then
            win.Decorated <- not win.Decorated
        elif e.Key = Keys.Insert then
            win.IsVisible <- false
            System.Threading.Tasks.Task.Delay(1000).ContinueWith(fun _ -> win.IsVisible <- true) |> ignore
        elif e.Key = Keys.F12 then
            win.Floating <- not win.Floating
        elif e.Key = Keys.Enter then
            win.Fullcreen <- not win.Fullcreen
    )

    win.WindowStateChanged.Add (fun s ->
        Log.warn "%A" s
    )

    win.KeyInput.Add(fun str ->
        Log.warn "input: %A" str
    )

    win.MouseDown.Add (fun e ->
        Log.warn "%A" e
    )

    win.MouseWheel.Add (fun e ->
        Log.warn "wheel: %A" e
    )

    win.KeyUp.Add(fun e ->
        Log.warn "%A" e
    )

    let createIcon (size : V2i) =
        let img = PixImage<byte>(Col.Format.RGBA, size)
        let gridSize = size / 2
        img.GetMatrix<C4b>().SetByCoord (fun (c : V2l) ->
            let tc = (V2d c + V2d.Half) / V2d img.Size
            let ndc = 2.0 * tc - V2d.II
            // if ndc.Length > 0.8 then C4b(0uy,0uy,0uy,0uy)
            // else C4b.VRVisGreen
            let color =
                let c = c / V2l gridSize
                if (c.X + c.Y) % 2L = 0L then C4b.VRVisGreen
                else C4b(0uy, 0uy, 0uy, 127uy)



            let l = 0.8
            let h = 0.9

            if ndc.Length <= l then
                color
            elif ndc.Length <= h then 
                let a = 1.0 - (ndc.Length - l) / (h-l)
                C4b(color.R, color.G, color.B, byte (float color.A * a))                
            else
                C4b(0uy,0uy,0uy,0uy)            
        ) |> ignore
        img

    let sizes = [| 16; 24; 32; 48; 64; 128; 256 |]

    let icon = sizes |> Array.map (fun s -> createIcon (V2i.II * s) :> PixImage) |> PixImageMipMap
    win.Icon <- Some icon

    let thread = 
        startThread (fun () ->
            while true do
                let line = Console.ReadLine()
                let win2 = 
                    app.CreateWindow {
                        title = "Dummy2"            
                        width = 200
                        height = 200
                        resizable = true
                        focus = true
                        refreshRate = 0
                        opengl = Some(4,1)
                        physicalSize = false
                        transparent = true
                    }
                win2.IsVisible <- true                
        )


    //win.Icon <- None
    app.Run(win)
    // Ag.initialize()
    // Aardvark.Init()

    // Window.create()


    0
