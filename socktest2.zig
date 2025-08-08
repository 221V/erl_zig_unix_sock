

const std = @import("std");

const c = @cImport({
  @cInclude("sys/socket.h");
  @cInclude("sys/un.h");
  @cInclude("unistd.h");
});

const SOCKET_PATH = "/tmp/socktest2.sock";


pub fn main() !void{
  std.fs.deleteFileAbsolute(SOCKET_PATH) catch {}; // delete old if exists
  
  const sock = c.socket(c.AF_UNIX, c.SOCK_DGRAM, 0); // create new TCP sock, Unix domain (TCP-like)
  if(sock == -1){
    std.debug.print("failed to create socket\n", .{});
    return error.SocketCreateFailed;
  }
  defer _ = c.close(sock);
  
  var addr: c.struct_sockaddr_un = undefined;
  addr.sun_family = c.AF_UNIX;
  
  if(SOCKET_PATH.len >= addr.sun_path.len){
    return error.SocketPathTooLong;
  }
  @memcpy(addr.sun_path[0..SOCKET_PATH.len], SOCKET_PATH);
  addr.sun_path[SOCKET_PATH.len] = 0;
  
  if(c.bind(sock, @ptrCast(&addr), @sizeOf(c.struct_sockaddr_un)) == -1){
    std.debug.print("failed to bind socket\n", .{});
    return error.SocketBindFailed;
  }
  
  std.debug.print("Zig UDP server is listening {s}\n", .{SOCKET_PATH});
  
  
  var buffer: [256]u8 = undefined;
  var sender_addr: c.struct_sockaddr_un = undefined;
  var addr_len: c.socklen_t = @sizeOf(c.struct_sockaddr_un);
  
  while(true){
    const n_bytes = c.recvfrom(
      sock,
      &buffer,
      buffer.len,
      0,
      @ptrCast(&sender_addr),
      &addr_len
    );
    
    if(n_bytes <= 0){
      std.debug.print("receive error\n", .{});
      continue;
    }
    
    const msg = buffer[0..@intCast(n_bytes)];
    std.debug.print("receive msg: {s}\n", .{msg});
    
    
    // process msg
    var reply_buffer: [128]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&reply_buffer);
    var writer = fbs.writer();
    
    if(std.mem.eql(u8, msg, "hello")){
      try writer.print("Hello World (by Zig) !", .{});
    
    }else if(std.mem.startsWith(u8, msg, "factorial ")){
      const num_str = msg["factorial ".len..];
      if(std.fmt.parseInt(usize, num_str, 10)) |num|{
        
        if(num > 34){ // max n for max factorial for u128
          try writer.print("error: input too large, max is 34", .{});
        }else{
          var fact: u128 = 1;
          var i = num;
          while(i > 1) : (i -= 1){ fact *= i; }
          try writer.print("factorial({d}) = {d}", .{ num, fact });
        }
      
      }else |_|{
        try writer.print("error: invalid number", .{});
      }
    
    }else{
      try writer.print("unknown command", .{});
    }
    
    const reply = fbs.getWritten();
    
    _ = c.sendto( // send responce
      sock,
      reply.ptr,
      reply.len,
      0,
      @ptrCast(&sender_addr),
      addr_len
    );
  }
}

