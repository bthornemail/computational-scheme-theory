/**
 * File Discovery - Utilities for finding and listing canvas files in the vault
 */

import { App, TFile } from 'obsidian';

/**
 * Find all .canvas files in the vault
 */
export async function findAllCanvasFiles(app: App): Promise<TFile[]> {
	const files = app.vault.getFiles();
	return files.filter(file => file.extension === 'canvas');
}

/**
 * Find a specific canvas file by name
 */
export async function findCanvasFile(app: App, name: string): Promise<TFile | null> {
	const canvasFiles = await findAllCanvasFiles(app);
	
	// Try exact match first
	let file = canvasFiles.find(f => f.basename === name || f.name === name);
	if (file) return file;
	
	// Try case-insensitive match
	file = canvasFiles.find(f => 
		f.basename.toLowerCase() === name.toLowerCase() || 
		f.name.toLowerCase() === name.toLowerCase()
	);
	
	return file || null;
}

/**
 * Get default canvas file (prefer "Documentation Flow Map.canvas")
 */
export async function getDefaultCanvasFile(app: App): Promise<TFile | null> {
	const defaultNames = [
		'Documentation Flow Map',
		'Computational Scheme Theory Documentation Map',
		'Computation Scheme Theory'
	];
	
	for (const name of defaultNames) {
		const file = await findCanvasFile(app, name);
		if (file) return file;
	}
	
	// If no default found, return first canvas file
	const allCanvasFiles = await findAllCanvasFiles(app);
	return allCanvasFiles.length > 0 ? allCanvasFiles[0] : null;
}

/**
 * Read canvas file content
 */
export async function readCanvasFile(app: App, file: TFile): Promise<string> {
	return await app.vault.read(file);
}

