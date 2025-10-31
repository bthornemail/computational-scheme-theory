/**
 * File Metadata Utilities - Get tags, frontmatter, and other metadata from Obsidian files
 */

import { App, TFile, CachedMetadata } from 'obsidian';

export interface FileMetadata {
	path: string;
	tags: string[];
	frontmatter: Record<string, any>;
	links: string[];
	aliases: string[];
	headings: Array<{ level: number; heading: string }>;
}

/**
 * Get metadata for a file
 */
export async function getFileMetadata(app: App, filePath: string): Promise<FileMetadata | null> {
	try {
		// Resolve file path
		const abstractFile = app.vault.getAbstractFileByPath(filePath);
		if (!abstractFile || !(abstractFile instanceof TFile)) {
			// Try to find file with different path formats
			const files = app.vault.getFiles();
			const file = files.find(f => 
				f.path === filePath || 
				f.basename === filePath ||
				f.name === filePath ||
				f.path.endsWith(filePath) ||
				f.path.includes(filePath)
			);
			
			if (!file) return null;
			
			return await extractMetadata(app, file);
		}

		return await extractMetadata(app, abstractFile);
	} catch (error) {
		console.error('Error getting file metadata:', error);
		return null;
	}
}

/**
 * Extract metadata from a TFile
 */
async function extractMetadata(app: App, file: TFile): Promise<FileMetadata> {
	const cache = app.metadataCache.getFileCache(file);
	
	const tags: string[] = [];
	const frontmatter: Record<string, any> = {};
	const links: string[] = [];
	const aliases: string[] = [];
	const headings: Array<{ level: number; heading: string }> = [];

	if (cache) {
		// Extract tags
		if (cache.tags) {
			cache.tags.forEach(tag => {
				if (tag.tag) {
					const tagName = tag.tag.startsWith('#') ? tag.tag.slice(1) : tag.tag;
					if (!tags.includes(tagName)) {
						tags.push(tagName);
					}
				}
			});
		}

		// Extract frontmatter
		if (cache.frontmatter) {
			Object.assign(frontmatter, cache.frontmatter);
			// Extract aliases from frontmatter
			if (cache.frontmatter.aliases) {
				const aliasArray = Array.isArray(cache.frontmatter.aliases) 
					? cache.frontmatter.aliases 
					: [cache.frontmatter.aliases];
				aliases.push(...aliasArray);
			}
			if (cache.frontmatter.alias) {
				aliases.push(cache.frontmatter.alias);
			}
		}

		// Extract links
		if (cache.links) {
			cache.links.forEach(link => {
				if (link.link && !links.includes(link.link)) {
					links.push(link.link);
				}
			});
		}

		// Extract headings
		if (cache.headings) {
			cache.headings.forEach(heading => {
				headings.push({
					level: heading.level,
					heading: heading.heading
				});
			});
		}
	}

	return {
		path: file.path,
		tags,
		frontmatter,
		links,
		aliases,
		headings
	};
}

/**
 * Update tags in a file
 */
export async function updateFileTags(
	app: App,
	filePath: string,
	newTags: string[]
): Promise<boolean> {
	try {
		const abstractFile = app.vault.getAbstractFileByPath(filePath);
		if (!abstractFile || !(abstractFile instanceof TFile)) {
			return false;
		}

		const content = await app.vault.read(abstractFile);
		
		// Parse frontmatter
		const frontmatterRegex = /^---\s*\n([\s\S]*?)\n---\s*\n/;
		const match = content.match(frontmatterRegex);
		
		let frontmatter: Record<string, any> = {};
		let body = content;
		
		if (match) {
			try {
				frontmatter = app.metadataCache.getFileCache(abstractFile)?.frontmatter || {};
				body = content.slice(match[0].length);
			} catch (e) {
				// Frontmatter parse failed, treat as no frontmatter
			}
		}

		// Update tags in frontmatter
		frontmatter.tags = newTags.length > 0 ? newTags : undefined;
		if (!frontmatter.tags) {
			delete frontmatter.tags;
		}

		// Update body: handle inline tags
		let newBody = body;
		
		// Remove existing inline tags (preserve text around them)
		// Match tags that are: standalone, at start of line, after spaces, or before spaces
		newBody = newBody.replace(/\s*#[a-zA-Z][\w/-]*/g, '');
		
		// Add new inline tags at the end of the document if any
		// (Obsidian supports both frontmatter and inline tags)
		
		// Reconstruct file
		let newContent = '';
		if (Object.keys(frontmatter).length > 0) {
			newContent += '---\n';
			for (const [key, value] of Object.entries(frontmatter)) {
				if (Array.isArray(value)) {
					newContent += `${key}:\n`;
					value.forEach(v => {
						newContent += `  - ${v}\n`;
					});
				} else if (typeof value === 'object' && value !== null) {
					newContent += `${key}:\n`;
					for (const [k, v] of Object.entries(value)) {
						newContent += `  ${k}: ${v}\n`;
					}
				} else {
					newContent += `${key}: ${value}\n`;
				}
			}
			newContent += '---\n';
		}
		newContent += newBody;

		await app.vault.modify(abstractFile, newContent);
		return true;
	} catch (error) {
		console.error('Error updating file tags:', error);
		return false;
	}
}

/**
 * Update frontmatter in a file
 */
export async function updateFileFrontmatter(
	app: App,
	filePath: string,
	frontmatterUpdates: Record<string, any>
): Promise<boolean> {
	try {
		const abstractFile = app.vault.getAbstractFileByPath(filePath);
		if (!abstractFile || !(abstractFile instanceof TFile)) {
			return false;
		}

		const content = await app.vault.read(abstractFile);
		
		// Parse frontmatter
		const frontmatterRegex = /^---\s*\n([\s\S]*?)\n---\s*\n/;
		const match = content.match(frontmatterRegex);
		
		let frontmatter: Record<string, any> = {};
		let body = content;
		
		if (match) {
			try {
				frontmatter = app.metadataCache.getFileCache(abstractFile)?.frontmatter || {};
				body = content.slice(match[0].length);
			} catch (e) {
				// Frontmatter parse failed, treat as no frontmatter
			}
		}

		// Update frontmatter
		Object.assign(frontmatter, frontmatterUpdates);

		// Reconstruct file
		let newContent = '---\n';
		for (const [key, value] of Object.entries(frontmatter)) {
			if (Array.isArray(value)) {
				newContent += `${key}:\n`;
				value.forEach(v => {
					newContent += `  - ${v}\n`;
				});
			} else if (typeof value === 'object' && value !== null) {
				newContent += `${key}:\n`;
				for (const [k, v] of Object.entries(value)) {
					newContent += `  ${k}: ${v}\n`;
				}
			} else {
				newContent += `${key}: ${value}\n`;
			}
		}
		newContent += '---\n';
		newContent += body;

		await app.vault.modify(abstractFile, newContent);
		return true;
	} catch (error) {
		console.error('Error updating file frontmatter:', error);
		return false;
	}
}

