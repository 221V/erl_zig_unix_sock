

const std = @import("std");

const c = @cImport({
  @cInclude("sys/socket.h");
  @cInclude("sys/un.h");
  @cInclude("unistd.h");
});

const SOCKET_PATH = "/tmp/socktest.sock";


pub fn main() !void{
  std.fs.deleteFileAbsolute(SOCKET_PATH) catch {}; // delete old if exists
  
  const sock = c.socket(c.AF_UNIX, c.SOCK_STREAM, 0); // create new TCP sock, Unix domain (TCP-like)
  if(sock == -1){
    std.debug.print("failed to create socket\n", .{});
    return error.SocketCreateFailed;
  }
  defer _ = c.close(sock);
  
  var addr: c.struct_sockaddr_un = undefined;
  addr.sun_family = c.AF_UNIX;
  
  if(SOCKET_PATH.len >= addr.sun_path.len){
    return error.PathTooLong;
  }
  @memcpy(addr.sun_path[0..SOCKET_PATH.len], SOCKET_PATH);
  addr.sun_path[SOCKET_PATH.len] = 0;
  
  if(c.bind(sock, @ptrCast(&addr), @sizeOf(c.struct_sockaddr_un)) == -1){
    std.debug.print("failed to bind socket\n", .{});
    return error.SocketBindFailed;
  }
  
  if(c.listen(sock, 1) == -1){
    std.debug.print("failed to listen socket\n", .{});
    return error.SocketListenFailed;
  }
  
  std.debug.print("Zig server is listening {s}\n", .{SOCKET_PATH});
  
  while(true){
    const client_fd = c.accept(sock, null, null);
    if(client_fd == -1){
      std.debug.print("accept error\n", .{});
      continue;
    }
    
    std.debug.print("client connected\n", .{});
    
    const file = std.fs.File{ .handle = @intCast(client_fd) };
    const in_stream = file.reader();
    const out_stream = file.writer();
    
    var buffer: [256]u8 = undefined;
    const n_bytes = in_stream.read(&buffer) catch |err|{ // read msg
      std.debug.print("read msg err: {any}\n", .{err});
      _ = c.close(client_fd);
      continue;
    };
    
    const msg = buffer[0..n_bytes];
    std.debug.print("receive msg: {s}\n", .{msg});
    
    
    // process msg
    var reply: [128]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&reply);
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
    
    _ = try out_stream.write(fbs.getWritten()); // send responce
    _ = c.close(client_fd);
  }
}

