import { Plugin, Notice, MarkdownView, PluginSettingTab, Setting, App } from 'obsidian';
import { EPISTEMIC_WORKFLOW_VIEW, EpistemicWorkflowView } from './EpistemicWorkflowView';
import { createNLPExtension } from './editorNLPPlugin';

interface EpistemicTopologicalWorkflowSettings {
	nlpEnabled: boolean;
	nlpShowBadges: boolean;
	nlpDebounceMs: number;
	nlpRealTimeAnalysis: boolean;
}

const DEFAULT_SETTINGS: EpistemicTopologicalWorkflowSettings = {
	nlpEnabled: true,
	nlpShowBadges: false,
	nlpDebounceMs: 500,
	nlpRealTimeAnalysis: true
}

export default class EpistemicTopologicalWorkflowPlugin extends Plugin {
	settings: EpistemicTopologicalWorkflowSettings;

	async onload() {
		await this.loadSettings();

		// Register the workspace view
		this.registerView(
			EPISTEMIC_WORKFLOW_VIEW,
			(leaf) => new EpistemicWorkflowView(leaf)
		);

		// Register editor extension for NLP analysis
		this.registerEditorExtension(
			createNLPExtension({
				enabled: this.settings.nlpEnabled,
				showBadges: this.settings.nlpShowBadges,
				debounceMs: this.settings.nlpDebounceMs,
				realTimeAnalysis: this.settings.nlpRealTimeAnalysis
			})
		);

		// Add ribbon icon to open the view
		const ribbonIconEl = this.addRibbonIcon('network', 'Open Epistemic Workflow', async () => {
			await this.activateView();
		});

		// Add command to open the view
		this.addCommand({
			id: 'open-epistemic-workflow',
			name: 'Open Epistemic Workflow View',
			callback: async () => {
				await this.activateView();
			}
		});

		// Add command to refresh the view
		this.addCommand({
			id: 'refresh-epistemic-workflow',
			name: 'Refresh Epistemic Workflow View',
			callback: async () => {
				const view = this.app.workspace.getActiveViewOfType(EpistemicWorkflowView);
				if (view) {
					await view.refresh();
					new Notice('Epistemic Workflow view refreshed');
				}
			}
		});

		// Add command to analyze document for entities
		this.addCommand({
			id: 'analyze-epistemic-entities',
			name: 'Analyze Document for Epistemic Entities',
			editorCallback: (editor, view: MarkdownView) => {
				new Notice('Entity analysis is running automatically. Check highlighted text.');
			}
		});

		// Add command to toggle NLP analysis
		this.addCommand({
			id: 'toggle-nlp-analysis',
			name: 'Toggle Epistemic Entity Highlighting',
			callback: async () => {
				this.settings.nlpEnabled = !this.settings.nlpEnabled;
				await this.saveSettings();
				new Notice(
					this.settings.nlpEnabled 
						? 'Epistemic entity highlighting enabled' 
						: 'Epistemic entity highlighting disabled'
				);
				// Reload editor extensions
				this.app.workspace.updateOptions();
			}
		});

		// Add settings tab
		this.addSettingTab(new EpistemicWorkflowSettingTab(this.app, this));
	}

	onunload() {
		this.app.workspace.detachLeavesOfType(EPISTEMIC_WORKFLOW_VIEW);
	}

	async loadSettings() {
		this.settings = Object.assign({}, DEFAULT_SETTINGS, await this.loadData());
	}

	async saveSettings() {
		await this.saveData(this.settings);
	}

	async activateView() {
		const { workspace } = this.app;
		let leaf = workspace.getLeavesOfType(EPISTEMIC_WORKFLOW_VIEW)[0];

		if (!leaf) {
			// Create a new leaf in the right sidebar
			const newLeaf = workspace.getRightLeaf(false);
			if (newLeaf) {
				await newLeaf.setViewState({
					type: EPISTEMIC_WORKFLOW_VIEW,
					active: true,
				});
				leaf = newLeaf;
			}
		}

		if (leaf) {
			workspace.revealLeaf(leaf);
		}
	}
}

class EpistemicWorkflowSettingTab extends PluginSettingTab {
	plugin: EpistemicTopologicalWorkflowPlugin;

	constructor(app: App, plugin: EpistemicTopologicalWorkflowPlugin) {
		super(app, plugin);
		this.plugin = plugin;
	}

	display(): void {
		const { containerEl } = this;

		containerEl.empty();
		containerEl.createEl('h2', { text: 'Epistemic Topological Workflow Settings' });

		// NLP Analysis Settings
		containerEl.createEl('h3', { text: 'NLP Entity Analysis' });

		new Setting(containerEl)
			.setName('Enable Entity Highlighting')
			.setDesc('Highlight epistemic entities in the editor')
			.addToggle(toggle => toggle
				.setValue(this.plugin.settings.nlpEnabled)
				.onChange(async (value) => {
					this.plugin.settings.nlpEnabled = value;
					await this.plugin.saveSettings();
					this.app.workspace.updateOptions();
				}));

		new Setting(containerEl)
			.setName('Show Entity Badges')
			.setDesc('Display small badges next to detected entities')
			.addToggle(toggle => toggle
				.setValue(this.plugin.settings.nlpShowBadges)
				.onChange(async (value) => {
					this.plugin.settings.nlpShowBadges = value;
					await this.plugin.saveSettings();
					this.app.workspace.updateOptions();
				}));

		new Setting(containerEl)
			.setName('Real-time Analysis')
			.setDesc('Analyze document as you type (debounced)')
			.addToggle(toggle => toggle
				.setValue(this.plugin.settings.nlpRealTimeAnalysis)
				.onChange(async (value) => {
					this.plugin.settings.nlpRealTimeAnalysis = value;
					await this.plugin.saveSettings();
					this.app.workspace.updateOptions();
				}));

		new Setting(containerEl)
			.setName('Analysis Debounce (ms)')
			.setDesc('Delay before analyzing after typing stops')
			.addSlider(slider => slider
				.setLimits(100, 2000, 100)
				.setValue(this.plugin.settings.nlpDebounceMs)
				.setDynamicTooltip()
				.onChange(async (value) => {
					this.plugin.settings.nlpDebounceMs = value;
					await this.plugin.saveSettings();
				}));
	}
}
