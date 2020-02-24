open System
open FSharp.Data.Adaptive
open Aardvark.Base
open Aardvark.Base.Rendering
open Glfw

[<EntryPoint;STAThread>]
let main argv = 

    let win = 
        Window.create {
            title = "Dummy"            
            width = 1024
            height = 768
            resizable = true
            focus = true
            refreshRate = 0
            opengl = Some(4,1)
            physicalSize = false
        }

    let name = win.GetKeyName(Silk.NET.GLFW.Keys.Slash)
    Log.warn "Slash: %s" name


    // win.MouseMove.Add (fun e ->
    //     win.Title <- sprintf "mouse: %s" (e.ToString("0.00"))
    // )

    win.FocusChanged.Add(fun e ->
        Log.warn "focus: %A" e
    )

    win.Resize.Add (fun s ->
        Log.warn "resize: %A" s    
    )    

    win.KeyDown.Add(fun e ->
        if not e.IsRepeat then
            if e.Key = Silk.NET.GLFW.Keys.Escape then win.Close()
            else Log.warn "down %A" e

        if e.Key = Silk.NET.GLFW.Keys.Right then win.WindowPosition <- win.WindowPosition + V2i(20, 0)
        elif e.Key = Silk.NET.GLFW.Keys.Left then win.WindowPosition <- win.WindowPosition - V2i(20, 0)
        elif e.Key = Silk.NET.GLFW.Keys.Up then win.WindowPosition <- win.WindowPosition - V2i(0, 20)
        elif e.Key = Silk.NET.GLFW.Keys.Down then win.WindowPosition <- win.WindowPosition + V2i(0, 20)
        elif e.Key = Silk.NET.GLFW.Keys.R then win.WindowSize <- V2i(50, 50)
        elif e.Key = Silk.NET.GLFW.Keys.T then win.WindowSize <- V2i(1024, 768)
        elif e.Key = Silk.NET.GLFW.Keys.Q then win.FramebufferSize <- V2i(50, 50)
        elif e.Key = Silk.NET.GLFW.Keys.W then win.FramebufferSize <- V2i(1024, 768)
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
        Log.warn "up %A" e.Key
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
    //win.Icon <- None
    win.Run()
    // Ag.initialize()
    // Aardvark.Init()

    // Window.create()


    0
