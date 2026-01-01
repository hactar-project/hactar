import { spawn } from "bun";

// --- CONFIGURATION ---
// Replace this with the command to run your agent.
// Example: ["./my-cli-agent", "--acp"]
// Example: ["bun", "run", "src/index.ts"]
const AGENT_CMD = ["./hactar", "--acp"];
console.log(`🚀 Starting Agent: ${AGENT_CMD.join(" ")}`);

const agent = spawn(AGENT_CMD, {
  stdin: "pipe",
  stdout: "pipe",
  stderr: "pipe",
});

// --- READER: Handle Output from Agent ---
let currentSessionId: string | null = null;

async function readStream(stream: ReadableStream, label: string) {
  const reader = stream.getReader();
  const decoder = new TextDecoder();

  try {
    while (true) {
      const { done, value } = await reader.read();
      if (done) break;
      const text = decoder.decode(value);

      // Try to capture sessionId
      const sessionMatch = text.match(/"sessionId"\s*:\s*"([^"]+)"/);
      if (sessionMatch) {
        currentSessionId = sessionMatch[1];
        console.log(`\n🔑 Captured Session ID: ${currentSessionId}`);
      }

      // Print raw output to console
      process.stdout.write(`\n${label}: ${text}`);
    }
  } catch (e) {
    console.error(`Error reading ${label}:`, e);
  }
}

// Start listening to agent output
if (agent.stdout) readStream(agent.stdout, "🟢 [AGENT]");
if (agent.stderr) readStream(agent.stderr, "🔴 [ERROR]");

// --- WRITER: Handle User Input ---
console.log("\n--- ACP Manual Tester (Bun) ---");
console.log("1. Type 'init' and hit ENTER to start.");
console.log("2. Type 'session/new' to create a session.");
console.log("3. Type 'session/prompt <text>' to send a prompt.");
console.log("4. Paste JSON message and hit ENTER.");
console.log("-----------------------------------\n");

async function handleInput() {
  // CORRECTED LINE: Use Bun.stdin.stream()
  const reader = Bun.stdin.stream().getReader();
  const decoder = new TextDecoder();
  let buffer = "";

  while (true) {
    const { done, value } = await reader.read();
    if (done) break;

    buffer += decoder.decode(value);

    // Try to process the buffer
    const trimmed = buffer.trim();
    if (!trimmed) continue;

    // 1. Try to parse as complete JSON (handles multi-line paste)
    try {
      // Only try parsing if it looks like an object/array
      if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
        const parsed = JSON.parse(trimmed);
        const jsonToSend = JSON.stringify(parsed);

        if (agent.stdin) {
          agent.stdin.write(new TextEncoder().encode(jsonToSend + "\n"));
          agent.stdin.flush();
        }
        buffer = ""; // Message sent, clear buffer
        continue;
      }
    } catch (e) {
      // Parsing failed. If it looks like JSON start, we wait for more data.
      if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
        continue;
      }
    }

    // 2. Handle commands or raw text (line-based)
    if (buffer.includes("\n")) {
      const lines = buffer.split("\n");
      // Process all complete lines
      while (lines.length > 1) {
        const line = lines.shift()!.trim();
        if (!line) continue;

        let jsonToSend: string | null = null;

        if (line === "init") {
          jsonToSend = JSON.stringify({
            jsonrpc: "2.0",
            id: 0,
            method: "initialize",
            params: {
              clientInfo: { name: "bun-tester", version: "1.0" },
            },
          });
          console.log(`📝 Sending initialize...`);
        } else if (line === "session/new") {
          jsonToSend = JSON.stringify({
            jsonrpc: "2.0",
            id: 1, // Simple increment or static for manual testing
            method: "session/new",
            params: {},
          });
          console.log(`📝 Sending session/new...`);
        } else if (line.startsWith("session/prompt ")) {
          if (!currentSessionId) {
            console.error(
              "⚠️ No Session ID captured yet! Run 'session/new' first.",
            );
            continue;
          }
          const promptText = line.substring("session/prompt ".length);
          jsonToSend = JSON.stringify({
            jsonrpc: "2.0",
            id: 2,
            method: "session/prompt",
            params: {
              sessionId: currentSessionId,
              prompt: [
                { "type": "text", "text": promptText },
              ],
            },
          });
          console.log(`📝 Sending session/prompt...`);
        } else {
          // Send raw line
          if (agent.stdin) {
            agent.stdin.write(new TextEncoder().encode(line + "\n"));
            agent.stdin.flush();
          }
        }

        if (jsonToSend && agent.stdin) {
          agent.stdin.write(new TextEncoder().encode(jsonToSend + "\n"));
          agent.stdin.flush();
        }
      }
      buffer = lines.join("\n");
    }
  }
}

handleInput();

// Cleanup on exit
process.on("SIGINT", () => {
  console.log("\n🛑 Stopping agent...");
  agent.kill();
  process.exit(0);
});
