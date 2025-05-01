use crate::web::{Web, InterferenceGraph, InstructionMap, InstructionIndex};
use std::collections::{BTreeMap, HashSet};
use petgraph::graphmap::UnGraphMap;
use petgraph::algo::connected_components;

pub struct RegisterAllocationGraph {
    pub webs: BTreeMap<i32, Web>,               // web_id -> Web
    pub interference: InterferenceGraph,
    pub instr_map: InstructionMap,
}

impl RegisterAllocationGraph {
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        dot.push_str("graph RegisterAllocation {\n");
        dot.push_str("  node [shape=box fontname=\"monospace\" fontsize=12 width=1.2 height=0.5];\n");
        dot.push_str("  graph [nodesep=1.0, ranksep=1.2];\n"); // TB layout for the overall graph

        // === Build connected components ===
        let mut graph = UnGraphMap::<i32, (), std::collections::hash_map::RandomState>::new();
        for (&u, neighbors) in &self.interference.edges {
            for &v in neighbors {
                graph.add_edge(u, v, ());
            }
            graph.add_node(u);
        }

        let mut visited = HashSet::new();
        let mut component_id = 0;

        for node in graph.nodes() {
            if visited.contains(&node) {
                continue;
            }

            let mut cluster = format!("  subgraph cluster_{} {{\n", component_id);
            cluster.push_str("    rankdir=LR;\n");
            cluster.push_str("    style=invis;\n");

            let mut stack = vec![node];
            while let Some(current) = stack.pop() {
                if !visited.insert(current) {
                    continue;
                }

                let web = &self.webs[&current];

                let defs = web
                    .defs
                    .iter()
                    .map(|idx| format!("B{}:I{}", idx.block_id, idx.instr_index))
                    .collect::<Vec<_>>()
                    .join("\\l");
                let uses = web
                    .uses
                    .iter()
                    .map(|idx| format!("B{}:I{}", idx.block_id, idx.instr_index))
                    .collect::<Vec<_>>()
                    .join("\\l");

                let label = format!(
                    "<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">\n\
                        <TR><TD COLSPAN=\"2\"><B>Web {}</B></TD></TR>\n\
                        <TR><TD><I>Var</I></TD><TD>{}</TD></TR>\n\
                        <TR><TD><I>Defs</I></TD><TD ALIGN=\"LEFT\">{}</TD></TR>\n\
                        <TR><TD><I>Uses</I></TD><TD ALIGN=\"LEFT\">{}</TD></TR>\n\
                        </TABLE>>",
                    web.id,
                    web.variable,
                    defs.replace("\\l", "<BR ALIGN=\"LEFT\"/>"),
                    uses.replace("\\l", "<BR ALIGN=\"LEFT\"/>"),
                );

                cluster.push_str(&format!("    {} [label={} shape=plaintext];\n", current, label));

                for neighbor in self.interference.neighbors(&current).unwrap_or(&HashSet::new()) {
                    if current < *neighbor {
                        cluster.push_str(&format!("    {} -- {};\n", current, neighbor));
                    }
                    stack.push(*neighbor);
                }
                
            }

            cluster.push_str("  }\n");
            dot.push_str(&cluster);
            component_id += 1;
        }

        dot.push_str("}\n");
        dot
    }

    pub fn render_dot(&self, output_format: &str) -> Vec<u8> {
        let dot_code = self.to_dot();

        let mut child = std::process::Command::new("dot")
            .arg(format!("-T{}", output_format))
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .spawn()
            .expect("Failed to spawn dot command");

        {
            let stdin = child.stdin.as_mut().expect("Failed to open stdin");
            use std::io::Write;
            stdin
                .write_all(dot_code.as_bytes())
                .expect("Failed to write DOT code to dot process");
        }

        let output = child
            .wait_with_output()
            .expect("Failed to read dot output");

        if output.status.success() {
            output.stdout
        } else {
            panic!(
                "Graphviz 'dot' command failed:\n{}",
                String::from_utf8_lossy(&output.stderr)
            );
        }
    }
}
