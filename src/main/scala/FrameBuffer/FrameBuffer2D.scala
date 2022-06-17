package hdmi.frameBuffer

import dsptools.numbers._

import chisel3._ 
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import chisel3.util._

case class FrameBuffer2DParameters(
  logo : Boolean,
  imageParams : ImageParameters,
  beatBytes   : Int
)

// FrameBuffer2D IO
class FrameBuffer2DIO(scalerWidth: Int) extends Bundle {
    // Timing signals
    val video_active = Input(Bool())
    val pixel_x = Input(UInt(16.W))
    val pixel_y = Input(UInt(16.W))
    // RGB data
    val rgb = Output(UInt(24.W))
    // Trigger signal
    val trigger = Output(Bool())
    // Load scaler
    val loadScaler = Output(Bool())
    // X_location
    val i_addr_x = Output(UInt(9.W))
    val i_addr_y = Output(UInt(8.W))
    // Input signal data
    val inData = Input(UInt(24.W))
    // Scaler signals
    val scaler_y = Input(UInt((scalerWidth+1).W))
    val i_scaler = Input(UInt(10.W))

    override def cloneType: this.type = FrameBuffer2DIO(scalerWidth).asInstanceOf[this.type]
}

object FrameBuffer2DIO {
  def apply(scalerWidth: Int): FrameBuffer2DIO = new FrameBuffer2DIO(scalerWidth)
}

class FrameBuffer2D(params: FrameBuffer2DParameters, scalerWidth: Int) extends Module {

    // FrameBuffer2D IO
    val io = IO(new FrameBuffer2DIO(scalerWidth))

    // RGB
    val rgb = RegInit(0.U(24.W))
    io.rgb := rgb

    // Color values
    val grey_px  = Cat( 63.U(8.W),  63.U(8.W),  63.U(8.W))
    val black_px = Cat(  0.U(8.W),   0.U(8.W),   0.U(8.W))
    val white_px = Cat(255.U(8.W), 255.U(8.W), 255.U(8.W))
    val red_px   = Cat(237.U(8.W),  26.U(8.W),  59.U(8.W))

    // Axis parameters
    val axis_size     = params.imageParams.axis_size
    val offset_left   = params.imageParams.offset_left
    val offset_top    = params.imageParams.offset_top
    val offset_right  = params.imageParams.offset_right
    val offset_bottom = params.imageParams.offset_bottom
    val div_x         = params.imageParams.div_x
    val div_size_x    = params.imageParams.div_size_x
    val div_y         = params.imageParams.div_y
    val div_size_y    = params.imageParams.div_size_y
    
    val x_start_sig = offset_left
    val x_end_sig   = offset_left + div_x*div_size_x
    
    val x_start = offset_left - axis_size
    val x_end   = offset_left + div_x*div_size_x + axis_size

    val y_start = offset_top - axis_size
    val y_end   = offset_top + div_size_y*div_y + axis_size

    val y_start_sig = offset_top
    val y_end_sig   = offset_top + div_size_y*div_y

    // Connect inputs video_active, pixel_x and pixel_y to regs
    val video_active = RegInit(false.B)
    val pixel_x = RegInit(0.U(16.W))
    val pixel_y = RegInit(0.U(16.W))

    video_active := io.video_active
    pixel_x := io.pixel_x
    pixel_y := io.pixel_y

    val inData = RegInit(0.U(24.W))
    inData := io.inData

    // Scaler registers
    val i_scaler = RegNext(RegNext(io.i_scaler, 0.U), 0.U)
    val scaler_y = RegInit(0.U((scalerWidth + 1).W))

    // relative location of coordinate X
    val w_temp_addrx = pixel_x - x_start_sig
    when ((pixel_x >= x_start_sig) && (pixel_x < x_end_sig)) {
        io.i_addr_x := w_temp_addrx(9,1) >> i_scaler
    }
    .otherwise {
        io.i_addr_x := 0.U
    }
    // relative location of coordinate Y
    val w_temp_addry = pixel_y - y_start_sig
    when ((pixel_y >= y_start_sig) && (pixel_y < y_end_sig)) {
        io.i_addr_y := w_temp_addry(8,1)
    }
    .otherwise {
        io.i_addr_y := 0.U
    }

    // load images
    // val img_div_x = new ImageRom("./src/main/resources/div_x.jpg")
    // val img_div_y = new ImageRom("./src/main/resources/div_y.jpg")
    // val img_0 = new ImageRom("./src/main/resources/0.jpg")
    // val img_1 = new ImageRom("./src/main/resources/1.jpg")
    // val img_2 = new ImageRom("./src/main/resources/2.jpg")
    // val img_3 = new ImageRom("./src/main/resources/3.jpg")
    // val img_4 = new ImageRom("./src/main/resources/4.jpg")
    // val img_5 = new ImageRom("./src/main/resources/5.jpg")
    // val img_6 = new ImageRom("./src/main/resources/6.jpg")
    // val img_7 = new ImageRom("./src/main/resources/7.jpg")
    // val img_8 = new ImageRom("./src/main/resources/8.jpg")
    // val img_9 = new ImageRom("./src/main/resources/9.jpg")

    // Logo conditions
    val logo_cond = RegInit(false.B)
    val logo_img  = RegInit(false.B)
    // if logo was enabled generate logo on screen
    if (params.logo) {
        // load image
        val img_novel = new ImageRom("./src/main/resources/novel.jpg")
        logo_cond := (((pixel_x > x_end + offset_right) && (pixel_x <= x_end + offset_right + img_novel.w)) && ((pixel_y > y_start) && (pixel_y <= y_start + img_novel.h)))
        when (logo_cond) {
            logo_img := img_novel.imageROM(pixel_y - y_start.U)(pixel_x - (x_end + offset_right).U)
        }
        .otherwise {
            logo_img := false.B
        }
    }

    // // divs conditions
    // val div_x_cond = RegInit(false.B)
    // div_x_cond := (((pixel_x > x_start) && (pixel_x <= x_start + img_div_x.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val div_y_cond = RegInit(false.B)
    // div_y_cond := (((pixel_x > x_start + img_div_x.w + 5*img_0.w) && (pixel_x <= x_start + img_div_x.w + 5*img_0.w + img_div_y.w)) && ((pixel_y > y_end + (offset_top - img_div_y.h)/2) && (pixel_y <= y_end + (offset_top - img_div_y.h)/2 + img_div_y.h)))
        
    // val div_x_img = Wire(Bool())
    // val div_y_img = Wire(Bool())
    // when (div_x_cond) {
    //     div_x_img := img_div_x.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - x_start.U)
    // }
    // .otherwise {
    //     div_x_img := false.B
    // }
    // when (div_y_cond) {
    //     div_y_img := img_div_y.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 5*img_0.w).U)
    // }
    // .otherwise {
    //     div_y_img := false.B
    // }

    // // Number conditions for div_x
    // val num_x_0_cond = RegInit(false.B)
    // num_x_0_cond := (((pixel_x > x_start + img_div_x.w + 0*img_0.w) && (pixel_x <= x_start + img_div_x.w + 1*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val num_x_1_cond = RegInit(false.B)
    // num_x_1_cond := (((pixel_x > x_start + img_div_x.w + 1*img_0.w) && (pixel_x <= x_start + img_div_x.w + 2*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val num_x_2_cond = RegInit(false.B)
    // num_x_2_cond := (((pixel_x > x_start + img_div_x.w + 2*img_0.w) && (pixel_x <= x_start + img_div_x.w + 3*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val num_x_3_cond = RegInit(false.B)
    // num_x_3_cond := (((pixel_x > x_start + img_div_x.w + 3*img_0.w) && (pixel_x <= x_start + img_div_x.w + 4*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))

    // val num_x_0 = Wire(Bool())
    // val num_x_1 = Wire(Bool())
    // val num_x_2 = Wire(Bool())
    // val num_x_3 = Wire(Bool())

    // when(i_scaler === 0.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     when (num_x_1_cond) {
    //         num_x_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_1 := true.B
    //     }
    //     when (num_x_2_cond) {
    //         num_x_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 2*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_2 := true.B
    //     }
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 1.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     when (num_x_1_cond) {
    //         num_x_1 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_1 := true.B
    //     }
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 2.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_3.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     when (num_x_1_cond) {
    //         num_x_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_1 := true.B
    //     }
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 3.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     when (num_x_1_cond) {
    //         num_x_1 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_1 := true.B
    //     }
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 4.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     num_x_1 := true.B
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 5.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     num_x_1 := true.B
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 6.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     num_x_1 := true.B
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .elsewhen (i_scaler === 7.U) {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     num_x_1 := true.B
    //     num_x_2 := true.B
    //     num_x_3 := true.B
    // }
    // .otherwise {
    //     when (num_x_0_cond) {
    //         num_x_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 0*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_0 := true.B
    //     }
    //     when (num_x_1_cond) {
    //         num_x_1 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 1*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_1 := true.B
    //     }
    //     when (num_x_2_cond) {
    //         num_x_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 2*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_2 := true.B
    //     }
    //     when (num_x_3_cond) {
    //         num_x_3 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + 3*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_x_3 := true.B
    //     }
    // }

    // // Number conditions for div_y
    // val num_y_0_cond = RegInit(false.B)
    // num_y_0_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 5*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 6*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val num_y_1_cond = RegInit(false.B)
    // num_y_1_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 6*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 7*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val num_y_2_cond = RegInit(false.B)
    // num_y_2_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 7*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 8*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))
    // val num_y_3_cond = RegInit(false.B)
    // num_y_3_cond := (((pixel_x > x_start + img_div_x.w + img_div_y.w + 8*img_0.w) && (pixel_x <= x_start + img_div_x.w + img_div_y.w + 9*img_0.w)) && ((pixel_y > y_end + (offset_top - img_div_x.h)/2) && (pixel_y <= y_end + (offset_top - img_div_x.h)/2 + img_div_x.h)))

    // val num_y_0 = Wire(Bool())
    // val num_y_1 = Wire(Bool())
    // val num_y_2 = Wire(Bool())
    // val num_y_3 = Wire(Bool())

    // when(scaler_y === 0.U || scaler_y === 8.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 1.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 2.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_5.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 3.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_5.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 4.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     when (num_y_3_cond) {
    //         num_y_3 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_3 := true.B
    //     }
    // }
    // .elsewhen (scaler_y === 5.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     when (num_y_3_cond) {
    //         num_y_3 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_3 := true.B
    //     }
    // }
    // .elsewhen (scaler_y === 6.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_9.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     when (num_y_3_cond) {
    //         num_y_3 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_3 := true.B
    //     }
    // }
    // .elsewhen (scaler_y === 7.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_9.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     when (num_y_3_cond) {
    //         num_y_3 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_3 := true.B
    //     }
    // }
    // .elsewhen (scaler_y === 9.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_3.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 10.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_6.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 11.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     num_y_1 := true.B
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 12.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_4.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     num_y_1 := true.B
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 13.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_2.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     num_y_1 := true.B
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 14.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_1.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     num_y_1 := true.B
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .elsewhen (scaler_y === 15.U) {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_0.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     num_y_1 := true.B
    //     num_y_2 := true.B
    //     num_y_3 := true.B
    // }
    // .otherwise {
    //     when (num_y_0_cond) {
    //         num_y_0 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 5*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_0 := true.B
    //     }
    //     when (num_y_1_cond) {
    //         num_y_1 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 6*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_1 := true.B
    //     }
    //     when (num_y_2_cond) {
    //         num_y_2 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 7*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_2 := true.B
    //     }
    //     when (num_y_3_cond) {
    //         num_y_3 := img_8.imageROM(pixel_y - (y_end + (offset_top - img_div_x.h)/2).U)(pixel_x - (x_start + img_div_x.w + img_div_y.w + 8*img_0.w).U)
    //     }
    //     .otherwise {
    //         num_y_3 := true.B
    //     }
    // }

    // FFT 2D signal condition
    val fft2d_cond = RegInit(false.B)
    // fft2d_cond := ((pixel_x > x_start_sig) && (pixel_x <= x_end_sig)) && ((pixel_y > y_start + axis_size/2) && (pixel_y <= y_end - axis_size/2))
    // fft2d_cond := RegNext((pixel_x >= x_start_sig) && (pixel_x < x_end_sig)) && ((pixel_y >= y_start_sig) && (pixel_y < y_end_sig))
    fft2d_cond := ((pixel_x >= x_start_sig) && (pixel_x < x_end_sig)) && ((pixel_y >= y_start_sig) && (pixel_y < y_end_sig))

    // Border line condition
    val border_cond = RegInit(false.B)
    border_cond := (((pixel_x >= x_start) && (pixel_x < x_end)) && (((pixel_y >= y_start) && (pixel_y < y_start + axis_size)) || ((pixel_y >= y_end - axis_size) && (pixel_y < y_end)))) ||
                   (((pixel_y >= y_start + axis_size) && (pixel_y < y_end - axis_size)) && (((pixel_x >= x_start) && (pixel_x < x_start + axis_size)) || ((pixel_x >= x_end - axis_size) && (pixel_x < x_end)))) 

    // Draw
    when (video_active) {
    // FrameBuffer2D
        when (fft2d_cond){
            rgb := inData // send calculated pixels
        }
        // Draw boarder lines
        .elsewhen (border_cond) {
            rgb := white_px // send white pixels
        }
        // Draw NovelIC logo
        .elsewhen (logo_cond) {
            when (logo_img) {
                rgb := white_px // send white_px pixels
            }
            .otherwise {
                 rgb := red_px // send red_px pixels
            }
        }
        // // Draw div_x and div_y
        // .elsewhen ((div_x_cond && (div_x_img === false.B)) || (div_y_cond && (div_y_img === false.B))) {
        //     rgb := white_px // send white_px pixels
        // }
        // // Draw numbers
        // .elsewhen ((num_x_0_cond && (num_x_0 === false.B)) || (num_x_1_cond && (num_x_1 === false.B)) || (num_x_2_cond && (num_x_2 === false.B)) || (num_x_3_cond && (num_x_3 === false.B))) {
        //     rgb := white_px // send white_px pixels
        // }
        // // Draw numbers
        // .elsewhen ((num_y_0_cond && (num_y_0 === false.B)) || (num_y_1_cond && (num_y_1 === false.B)) || (num_y_2_cond && (num_y_2 === false.B)) || (num_y_3_cond && (num_y_3 === false.B))) {
        //     rgb := white_px // send white_px pixels
        // }
        // Otherwise send black pixels
        .otherwise {
            rgb := black_px // send black pixels
        }
    }
    .otherwise {
        rgb := black_px // send black pixels
    }
    
    // Generate trigger signal
    when (video_active) {
        // Start
        when((pixel_y === 0.U) && (pixel_x < x_start)) {
            io.trigger     := true.B
            scaler_y     := io.scaler_y
        }
        .otherwise {
            io.trigger := false.B
        }
        // Load scaler
        when (pixel_y === (params.imageParams.v_video-1).U) {
            io.loadScaler := true.B
        }
        .otherwise {
            io.loadScaler := false.B
        }
    }
    .otherwise{
        io.trigger      := false.B
        io.loadScaler := false.B
    }
}

class HD1080p2D {
    val params = ImageParameters (
        // image parameters
        h_video       = 1920,
        h_fp          = 88,
        h_sync        = 44,
        h_bp          = 148,
        h_total       = 2200,
        v_video       = 1080,
        v_fp          = 4,
        v_sync        = 5,
        v_bp          = 36,
        v_total       = 1125,
        h_pol         = 1,
        v_pol         = 1,
        active        = 1,
        // offset parameters
        offset_top    = 64,
        offset_bottom = 64,
        offset_left   = 64 + 320,
        offset_right  = 64,
        // graph parameters
        div_size_x    = 128,
        div_x         = 8,
        div_size_y    = 128,
        div_y         = 4,
        div_size_y_2  = 32,
        div_y_2       = 12,
        axis_size     = 2,
    )
}
object FrameBuffer2DApp extends App
{
  // here just define parameters
  val params = FrameBuffer2DParameters(
    logo = false,
    imageParams = (new HD1080p2D).params,
    beatBytes = 4
  )

  (new ChiselStage).execute(Array("--target-dir", "verilog/FrameBuffer2D"), Seq(ChiselGeneratorAnnotation(() => new FrameBuffer2D(params, 3))))
}