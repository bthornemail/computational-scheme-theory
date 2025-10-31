/**
 * Epistemic Workflow View - Obsidian workspace view for displaying epistemic topology
 */

import { ItemView, WorkspaceLeaf } from 'obsidian';
import * as React from 'react';
import { Root, createRoot } from 'react-dom/client';
import { ReactView } from './ReactView';
import { CanvasData, parseCanvasFile } from './canvasParser';
import { getDefaultCanvasFile, readCanvasFile } from './fileDiscovery';

export const EPISTEMIC_WORKFLOW_VIEW = 'epistemic-workflow-view';

export class EpistemicWorkflowView extends ItemView {
	root: Root | null = null;
	canvasData: CanvasData | null = null;

	constructor(leaf: WorkspaceLeaf) {
		super(leaf);
	}

	getViewType() {
		return EPISTEMIC_WORKFLOW_VIEW;
	}

	getDisplayText() {
		return 'Epistemic Workflow';
	}

	getIcon() {
		return 'network';
	}

	async onOpen() {
		await this.loadCanvasData();
		this.root = createRoot(this.contentEl);
		this.renderView();
	}

	async onClose() {
		this.root?.unmount();
		this.root = null;
	}

	async loadCanvasData() {
		const defaultFile = await getDefaultCanvasFile(this.app);
		if (!defaultFile) {
			this.canvasData = null;
			return;
		}

		const content = await readCanvasFile(this.app, defaultFile);
		this.canvasData = parseCanvasFile(content);
	}

	renderView() {
		if (!this.root) return;

		this.root.render(
			React.createElement(
				React.StrictMode,
				null,
				React.createElement(ReactView, {
					canvasData: this.canvasData,
					app: this.app
				})
			)
		);
	}

	async refresh() {
		await this.loadCanvasData();
		this.renderView();
	}
}

