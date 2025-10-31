import * as React from 'react';
import { App } from 'obsidian';
import { CanvasData, CanvasNode, CanvasEdge, getEpistemicState, getEpistemicColorClass, getEpistemicLabel } from './canvasParser';
import './styles.css';

interface ReactViewProps {
	canvasData: CanvasData | null;
	app: App;
}

export const ReactView: React.FC<ReactViewProps> = ({ canvasData, app }: ReactViewProps) => {
	const [selectedNode, setSelectedNode] = React.useState<string | null>(null);

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

	const handleNodeClick = (node: CanvasNode) => {
		if (node.type === 'file' && node.file) {
			app.workspace.openLinkText(node.file, '', false);
		}
		setSelectedNode(node.id === selectedNode ? null : node.id);
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
		</div>
	);
  };
