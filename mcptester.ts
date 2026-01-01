import { spawn } from "bun";

// --- CONFIGURATION ---
// Replace this with the command to run your MCP server.
// Example: ["node", "my-mcp-server.js"]
// Example: ["bun", "run", "src/mcp-server.ts"]
const SERVER_CMD = ["./hactar", "--mcp"];
console.log(`🚀 Starting MCP Server: ${SERVER_CMD.join(" ")}`);

const server = spawn(SERVER_CMD, {
    stdin: "pipe",
    stdout: "pipe",
    stderr: "pipe",
});

// --- READER: Handle Output from Server ---
async function readStream(stream: ReadableStream, label: string) {
    const reader = stream.getReader();
    const decoder = new TextDecoder();

    try {
        while (true) {
            const { done, value } = await reader.read();
            if (done) break;
            const text = decoder.decode(value);
            process.stdout.write(`\n${label}: ${text}`);
        }
    } catch (e) {
        console.error(`Error reading ${label}:`, e);
    }
}

// Start listening to server output
if (server.stdout) readStream(server.stdout, "🟢 [SERVER]");
if (server.stderr) readStream(server.stderr, "🔴 [ERROR]");

// --- Helper: Send JSON-RPC to server ---
let requestId = 0;

function send(obj: object) {
    const jsonToSend = JSON.stringify(obj);
    console.log(`📤 Sending: ${jsonToSend}`);
    if (server.stdin) {
        server.stdin.write(new TextEncoder().encode(jsonToSend + "\n"));
        server.stdin.flush();
    }
}

// --- WRITER: Handle User Input ---
console.log("\n--- MCP Manual Tester (Bun) ---");
console.log("Commands:");
console.log("  init          - Send initialize request");
console.log("  initialized   - Send initialized notification");
console.log("  list-tools    - Send tools/list request");
console.log("  list-resources- Send resources/list request");
console.log("  list-prompts  - Send prompts/list request");
console.log("  call <name> <json-args> - Call a tool");
console.log("  read <uri>    - Read a resource");
console.log("  get-prompt <name> <json-args> - Get a prompt");
console.log("  ping          - Send ping request");
console.log("  Or paste raw JSON-RPC to send directly.");
console.log("---------------------------------\n");

async function handleInput() {
    const reader = Bun.stdin.stream().getReader();
    const decoder = new TextDecoder();
    let buffer = "";

    while (true) {
        const { done, value } = await reader.read();
        if (done) break;

        buffer += decoder.decode(value);

        const trimmed = buffer.trim();
        if (!trimmed) continue;

        // 1. Try to parse as complete JSON (handles multi-line paste)
        try {
            if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
                const parsed = JSON.parse(trimmed);
                send(parsed);
                buffer = "";
                continue;
            }
        } catch (e) {
            if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
                continue; // wait for more data
            }
        }

        // 2. Handle commands (line-based)
        if (buffer.includes("\n")) {
            const lines = buffer.split("\n");
            while (lines.length > 1) {
                const line = lines.shift()!.trim();
                if (!line) continue;

                if (line === "init") {
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "initialize",
                        params: {
                            protocolVersion: "2024-11-05",
                            capabilities: {},
                            clientInfo: { name: "mcp-tester", version: "1.0.0" }
                        }
                    });
                } else if (line === "initialized") {
                    send({
                        jsonrpc: "2.0",
                        method: "notifications/initialized"
                    });
                } else if (line === "list-tools") {
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "tools/list",
                        params: {}
                    });
                } else if (line === "list-resources") {
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "resources/list",
                        params: {}
                    });
                } else if (line === "list-prompts") {
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "prompts/list",
                        params: {}
                    });
                } else if (line === "ping") {
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "ping"
                    });
                } else if (line.startsWith("call ")) {
                    const rest = line.slice(5).trim();
                    const spaceIdx = rest.indexOf(" ");
                    const toolName = spaceIdx === -1 ? rest : rest.slice(0, spaceIdx);
                    let args = {};
                    if (spaceIdx !== -1) {
                        try {
                            args = JSON.parse(rest.slice(spaceIdx + 1));
                        } catch (e) {
                            console.error("❌ Invalid JSON args:", e);
                            continue;
                        }
                    }
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "tools/call",
                        params: { name: toolName, arguments: args }
                    });
                } else if (line.startsWith("read ")) {
                    const uri = line.slice(5).trim();
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "resources/read",
                        params: { uri }
                    });
                } else if (line.startsWith("get-prompt ")) {
                    const rest = line.slice(11).trim();
                    const spaceIdx = rest.indexOf(" ");
                    const promptName = spaceIdx === -1 ? rest : rest.slice(0, spaceIdx);
                    let args = {};
                    if (spaceIdx !== -1) {
                        try {
                            args = JSON.parse(rest.slice(spaceIdx + 1));
                        } catch (e) {
                            console.error("❌ Invalid JSON args:", e);
                            continue;
                        }
                    }
                    send({
                        jsonrpc: "2.0",
                        id: ++requestId,
                        method: "prompts/get",
                        params: { name: promptName, arguments: args }
                    });
                } else {
                    // Send raw line
                    if (server.stdin) {
                        server.stdin.write(new TextEncoder().encode(line + "\n"));
                        server.stdin.flush();
                    }
                }
            }
            buffer = lines.join("\n");
        }
    }
}

handleInput();

// Cleanup on exit
process.on("SIGINT", () => {
    console.log("\n🛑 Stopping server...");
    server.kill();
    process.exit(0);
});
