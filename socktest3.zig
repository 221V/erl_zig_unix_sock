
const std = @import("std");

extern fn __errno_location() *c_int;

fn getErrno() c_int{
  return __errno_location().*;
}

fn cstr_to_slice(ptr: [*:0]const u8) []const u8{ // c string to zig slice
  const len = c.strlen(ptr);
  return ptr[0..len];
}

const c = @cImport({
  @cInclude("sys/socket.h");
  @cInclude("sys/un.h");
  @cInclude("unistd.h");
  @cInclude("string.h");
});

const SOCKET_PATH = "/tmp/socktest3.sock";


pub fn main() !void{
  std.fs.deleteFileAbsolute(SOCKET_PATH) catch {}; // delete old if exists
  
  const sock = c.socket(c.AF_UNIX, c.SOCK_SEQPACKET, 0); // create new Unix domain sock (seqpacket)
  if(sock == -1){
    std.debug.print("failed to create socket\n", .{});
    return error.SocketCreateFailed;
  }
  defer _ = c.close(sock);
  
  var addr: c.struct_sockaddr_un = undefined;
  addr.sun_family = c.AF_UNIX;
  
  if(SOCKET_PATH.len >= addr.sun_path.len){
    std.debug.print("socket path too long\n", .{});
    return error.SocketPathTooLong;
  }
  @memcpy(addr.sun_path[0..SOCKET_PATH.len], SOCKET_PATH);
  addr.sun_path[SOCKET_PATH.len] = 0;
  
  if(c.bind(sock, @ptrCast(&addr), @sizeOf(c.struct_sockaddr_un)) == -1){
    const err = getErrno();
    const msg = c.strerror(err);
    std.debug.print("failed to bind socket: {} ({s})\n", .{ err, cstr_to_slice(msg) });
    return error.SocketBindFailed;
  }
  
  if(c.listen(sock, 5) == -1){
    const err = getErrno();
    const msg = c.strerror(err);
    std.debug.print("failed to listen on socket: {} ({s})\n", .{ err, cstr_to_slice(msg) });
    return error.SocketListenFailed;
  }
  
  std.debug.print("Zig SEQPACKET server is listening {s}\n", .{SOCKET_PATH});
  
  
  var buffer: [256]u8 = undefined;
  
  while(true){
    var client_addr: c.struct_sockaddr_un = undefined;
    var addr_len: c.socklen_t = @sizeOf(c.struct_sockaddr_un);
    
    const client_fd = c.accept(sock, @ptrCast(&client_addr), &addr_len);
    if(client_fd == -1){
      std.debug.print("accept failed\n", .{});
      continue;
    }
    
    while(true){
      const n_bytes = c.recv(client_fd, &buffer, buffer.len, 0);
      if(n_bytes <= 0){ // client disconnected or error
        break;
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
      
      
      if(c.send(client_fd, reply.ptr, reply.len, 0) == -1){ // send responce or err
        std.debug.print("send responce failed\n", .{});
        break;
      }
    
    } // end while for messages for client
    
    _ = c.close(client_fd);
    //std.debug.print("client ok disconnected\n", .{});
  
  } // end while for clients
}

