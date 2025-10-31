/**
 * Canvas Parser - Parses JSON Canvas files and maps epistemic states
 * Based on the Epistemic Topological Workflow research document
 */

export interface CanvasNode {
	id: string;
	type: 'text' | 'file' | 'link' | 'group';
	x: number;
	y: number;
	width: number;
	height: number;
	color?: string;
	text?: string;
	file?: string;
	label?: string;
}

export interface CanvasEdge {
	id: string;
	fromNode: string;
	toNode: string;
	label?: string;
	color?: string;
	fromSide?: string;
	toSide?: string;
}

export interface CanvasData {
	version?: string;
	nodes: CanvasNode[];
	edges?: CanvasEdge[];
	groups?: CanvasNode[];
	connections?: CanvasEdge[];
}

export type EpistemicState = 
	| 'unknown-unknown'      // Red - "1"
	| 'known-unknown'        // Orange - "2"
	| 'transitional'         // Yellow - "3"
	| 'known-known'          // Green - "4"
	| 'meta-knowledge'       // Cyan - "5"
	| 'foundational';       // Purple - "6"

/**
 * Maps Obsidian canvas color preset to epistemic state
 * According to research document section 4.2
 */
export function getEpistemicState(color?: string): EpistemicState {
	switch (color) {
		case '1':
			return 'unknown-unknown';      // Red
		case '2':
			return 'known-unknown';        // Orange
		case '3':
			return 'transitional';         // Yellow
		case '4':
			return 'known-known';          // Green
		case '5':
			return 'meta-knowledge';       // Cyan
		case '6':
			return 'foundational';         // Purple
		default:
			return 'transitional';         // Default to transitional
	}
}

/**
 * Get CSS color class for epistemic state
 */
export function getEpistemicColorClass(state: EpistemicState): string {
	return `epistemic-${state}`;
}

/**
 * Parse JSON Canvas file content
 */
export function parseCanvasFile(content: string): CanvasData | null {
	try {
		const data = JSON.parse(content) as CanvasData;
		
		// Normalize edges (some canvas files use 'edges', others use 'connections')
		if (!data.edges && data.connections) {
			data.edges = data.connections;
		}
		
		return data;
	} catch (error) {
		console.error('Failed to parse canvas file:', error);
		return null;
	}
}

/**
 * Get display label for epistemic state
 */
export function getEpistemicLabel(state: EpistemicState): string {
	switch (state) {
		case 'unknown-unknown':
			return 'Unknown Unknown';
		case 'known-unknown':
			return 'Known Unknown';
		case 'transitional':
			return 'Transitional';
		case 'known-known':
			return 'Known Known';
		case 'meta-knowledge':
			return 'Meta-knowledge';
		case 'foundational':
			return 'Foundational';
	}
}

