import * as React from 'react';
import { App } from 'obsidian';
import { CanvasData, CanvasNode, CanvasEdge, getEpistemicState, getEpistemicColorClass, getEpistemicLabel } from './canvasParser';
import { getFileMetadata, updateFileTags, updateFileFrontmatter, FileMetadata } from './fileMetadata';
import './styles.css';

interface ReactViewProps {
	canvasData: CanvasData | null;
	app: App;
}

export const ReactView: React.FC<ReactViewProps> = ({ canvasData, app }: ReactViewProps) => {
	const [selectedNode, setSelectedNode] = React.useState<string | null>(null);
	const [selectedFileMetadata, setSelectedFileMetadata] = React.useState<FileMetadata | null>(null);
	const [editingTags, setEditingTags] = React.useState(false);
	const [tagInput, setTagInput] = React.useState('');
	const [editingFrontmatter, setEditingFrontmatter] = React.useState(false);
	const [frontmatterInput, setFrontmatterInput] = React.useState('');

	if (!canvasData || !canvasData.nodes || canvasData.nodes.length === 0) {
		return (
			<div className="epistemic-workflow-container">
				<div className="epistemic-empty-state">
					<h3>No Canvas Data</h3>
					<p>No canvas files found. Create a .canvas file in your vault to visualize epistemic workflow.</p>
				</div>
			</div>
		);
	}

	const nodes = canvasData.nodes || [];
	const edges = canvasData.edges || [];

	// Calculate viewport bounds for centering
	const minX = Math.min(...nodes.map((n: CanvasNode) => n.x));
	const maxX = Math.max(...nodes.map((n: CanvasNode) => n.x + n.width));
	const minY = Math.min(...nodes.map((n: CanvasNode) => n.y));
	const maxY = Math.max(...nodes.map((n: CanvasNode) => n.y + n.height));

	const handleNodeClick = async (node: CanvasNode) => {
		if (node.type === 'file' && node.file) {
			// Resolve file path properly
			const abstractFile = app.vault.getAbstractFileByPath(node.file);
			if (abstractFile) {
				app.workspace.openLinkText(node.file, '', false);
			} else {
				// Try to find file with different path formats
				const files = app.vault.getFiles();
				const file = files.find(f => {
					if (!node.file) return false;
					return f.path === node.file || 
						f.basename === node.file ||
						f.name === node.file ||
						f.path.endsWith(node.file) ||
						f.path.includes(node.file);
				});
				
				if (file) {
					app.workspace.openLinkText(file.path, '', false);
				}
			}
			
			// Load metadata for selected file
			if (node.file) {
				const metadata = await getFileMetadata(app, node.file);
				setSelectedFileMetadata(metadata);
			}
		} else {
			setSelectedFileMetadata(null);
		}
		setSelectedNode(node.id === selectedNode ? null : node.id);
	};

	const handleSaveTags = async () => {
		if (!selectedFileMetadata) return;
		
		const tags = tagInput
			.split(',')
			.map(t => t.trim())
			.filter(t => t.length > 0)
			.map(t => t.startsWith('#') ? t.slice(1) : t);
		
		const success = await updateFileTags(app, selectedFileMetadata.path, tags);
		if (success) {
			setSelectedFileMetadata({ ...selectedFileMetadata, tags });
			setEditingTags(false);
			// Trigger metadata cache refresh
			app.metadataCache.trigger('changed', app.vault.getAbstractFileByPath(selectedFileMetadata.path)!);
		}
	};

	const handleSaveFrontmatter = async () => {
		if (!selectedFileMetadata) return;
		
		try {
			const frontmatter = JSON.parse(frontmatterInput);
			const success = await updateFileFrontmatter(app, selectedFileMetadata.path, frontmatter);
			if (success) {
				setSelectedFileMetadata({ ...selectedFileMetadata, frontmatter });
				setEditingFrontmatter(false);
				// Trigger metadata cache refresh
				app.metadataCache.trigger('changed', app.vault.getAbstractFileByPath(selectedFileMetadata.path)!);
			}
		} catch (error) {
			console.error('Invalid JSON in frontmatter:', error);
		}
	};

	const renderNode = (node: CanvasNode) => {
		const epistemicState = getEpistemicState(node.color);
		const colorClass = getEpistemicColorClass(epistemicState);
		const isSelected = selectedNode === node.id;
		const isFileNode = node.type === 'file';
		const isGroupNode = node.type === 'group';

		// Skip group nodes for now (they're handled differently)
		if (isGroupNode) {
			return null;
		}

		const style: React.CSSProperties = {
			left: `${node.x - minX}px`,
			top: `${node.y - minY}px`,
			width: `${node.width}px`,
			height: `${node.height}px`,
		};

		const nodeClass = `epistemic-node ${colorClass} ${isSelected ? 'selected' : ''} ${isFileNode ? 'file-node' : 'text-node'}`;

		return (
			<div
				key={node.id}
				className={nodeClass}
				style={style}
				onClick={() => handleNodeClick(node)}
				title={isFileNode ? `Click to open: ${node.file}` : node.text?.substring(0, 100)}
			>
				{isFileNode && (
					<div className="epistemic-node-icon">📄</div>
				)}
				<div className="epistemic-node-content">
					{node.type === 'file' ? (
						<div className="epistemic-file-name">{node.file}</div>
					) : (
						<div 
							className="epistemic-node-text" 
							dangerouslySetInnerHTML={{ 
								__html: node.text?.replace(/\n/g, '<br>') || '' 
							}} 
						/>
					)}
				</div>
				<div className="epistemic-node-state-label">
					{getEpistemicLabel(epistemicState)}
				</div>
			</div>
		);
	};

	const renderEdge = (edge: CanvasEdge) => {
		const fromNode = nodes.find((n: CanvasNode) => n.id === edge.fromNode);
		const toNode = nodes.find((n: CanvasNode) => n.id === edge.toNode);

		if (!fromNode || !toNode) return null;

		// Calculate edge positions
		const fromX = fromNode.x + fromNode.width / 2;
		const fromY = fromNode.y + fromNode.height / 2;
		const toX = toNode.x + toNode.width / 2;
		const toY = toNode.y + toNode.height / 2;

		const dx = toX - fromX;
		const dy = toY - fromY;
		const length = Math.sqrt(dx * dx + dy * dy);
		const angle = Math.atan2(dy, dx) * 180 / Math.PI;

		const edgeStyle: React.CSSProperties = {
			left: `${fromX - minX}px`,
			top: `${fromY - minY}px`,
			width: `${length}px`,
			transform: `rotate(${angle}deg)`,
			transformOrigin: '0 0',
		};

		return (
			<div
				key={edge.id}
				className="epistemic-edge"
				style={edgeStyle}
				title={edge.label || ''}
			>
				{edge.label && (
					<div className="epistemic-edge-label">{edge.label}</div>
				)}
			</div>
		);
	};

	return (
		<div className="epistemic-workflow-container">
			<div className="epistemic-workflow-header">
				<h3>Epistemic Topological Workflow</h3>
				<div className="epistemic-legend">
					<div className="epistemic-legend-item">
						<div className="epistemic-legend-color epistemic-unknown-unknown"></div>
						<span>Unknown Unknown</span>
					</div>
					<div className="epistemic-legend-item">
						<div className="epistemic-legend-color epistemic-known-unknown"></div>
						<span>Known Unknown</span>
					</div>
					<div className="epistemic-legend-item">
						<div className="epistemic-legend-color epistemic-transitional"></div>
						<span>Transitional</span>
					</div>
					<div className="epistemic-legend-item">
						<div className="epistemic-legend-color epistemic-known-known"></div>
						<span>Known Known</span>
					</div>
					<div className="epistemic-legend-item">
						<div className="epistemic-legend-color epistemic-meta-knowledge"></div>
						<span>Meta-knowledge</span>
					</div>
					<div className="epistemic-legend-item">
						<div className="epistemic-legend-color epistemic-foundational"></div>
						<span>Foundational</span>
					</div>
				</div>
			</div>
			<div className="epistemic-workflow-viewport">
				<div 
					className="epistemic-workflow-canvas"
					style={{
						width: `${maxX - minX}px`,
						height: `${maxY - minY}px`,
						position: 'relative'
					}}
				>
					{edges.map(renderEdge)}
					{nodes.map(renderNode)}
				</div>
			</div>
			{selectedFileMetadata && (
				<div className="epistemic-file-details">
					<div className="epistemic-file-details-header">
						<h4>File Details: {selectedFileMetadata.path}</h4>
						<button 
							className="epistemic-close-button"
							onClick={() => {
								setSelectedNode(null);
								setSelectedFileMetadata(null);
							}}
						>
							×
						</button>
					</div>
					
					<div className="epistemic-file-details-content">
						{/* Tags Section */}
						<div className="epistemic-metadata-section">
							<div className="epistemic-metadata-header">
								<h5>Tags</h5>
								{!editingTags ? (
									<button 
										className="epistemic-edit-button"
										onClick={() => {
											setTagInput(selectedFileMetadata.tags.join(', '));
											setEditingTags(true);
										}}
									>
										Edit
									</button>
								) : (
									<div className="epistemic-edit-controls">
										<button 
											className="epistemic-save-button"
											onClick={handleSaveTags}
										>
											Save
										</button>
										<button 
											className="epistemic-cancel-button"
											onClick={() => setEditingTags(false)}
										>
											Cancel
										</button>
									</div>
								)}
							</div>
							{editingTags ? (
								<input
									type="text"
									className="epistemic-tag-input"
									value={tagInput}
									onChange={(e) => setTagInput(e.target.value)}
									placeholder="Enter tags separated by commas"
								/>
							) : (
								<div className="epistemic-tags-list">
									{selectedFileMetadata.tags.length > 0 ? (
										selectedFileMetadata.tags.map((tag, idx) => (
											<span key={idx} className="epistemic-tag">
												#{tag}
											</span>
										))
									) : (
										<span className="epistemic-no-tags">No tags</span>
									)}
								</div>
							)}
						</div>

						{/* Frontmatter Section */}
						<div className="epistemic-metadata-section">
							<div className="epistemic-metadata-header">
								<h5>Frontmatter</h5>
								{!editingFrontmatter ? (
									<button 
										className="epistemic-edit-button"
										onClick={() => {
											setFrontmatterInput(JSON.stringify(selectedFileMetadata.frontmatter, null, 2));
											setEditingFrontmatter(true);
										}}
									>
										Edit
									</button>
								) : (
									<div className="epistemic-edit-controls">
										<button 
											className="epistemic-save-button"
											onClick={handleSaveFrontmatter}
										>
											Save
										</button>
										<button 
											className="epistemic-cancel-button"
											onClick={() => setEditingFrontmatter(false)}
										>
											Cancel
										</button>
									</div>
								)}
							</div>
							{editingFrontmatter ? (
								<textarea
									className="epistemic-frontmatter-input"
									value={frontmatterInput}
									onChange={(e) => setFrontmatterInput(e.target.value)}
									placeholder="Enter frontmatter as JSON"
									rows={10}
								/>
							) : (
								<pre className="epistemic-frontmatter-display">
									{Object.keys(selectedFileMetadata.frontmatter).length > 0 
										? JSON.stringify(selectedFileMetadata.frontmatter, null, 2)
										: 'No frontmatter'
									}
								</pre>
							)}
						</div>

						{/* Links Section */}
						{selectedFileMetadata.links.length > 0 && (
							<div className="epistemic-metadata-section">
								<h5>Links ({selectedFileMetadata.links.length})</h5>
								<div className="epistemic-links-list">
									{selectedFileMetadata.links.map((link, idx) => (
										<button
											key={idx}
											className="epistemic-link-button"
											onClick={() => app.workspace.openLinkText(link, '', false)}
										>
											{link}
										</button>
									))}
								</div>
							</div>
						)}

						{/* Headings Section */}
						{selectedFileMetadata.headings.length > 0 && (
							<div className="epistemic-metadata-section">
								<h5>Headings ({selectedFileMetadata.headings.length})</h5>
								<ul className="epistemic-headings-list">
									{selectedFileMetadata.headings.map((heading, idx) => (
										<li key={idx} className={`epistemic-heading-level-${heading.level}`}>
											{heading.heading}
										</li>
									))}
								</ul>
							</div>
						)}
					</div>
				</div>
			)}
		</div>
	);
  };
