/**
 * Editor NLP Plugin - CodeMirror extension for epistemic entity analysis
 */

import { ViewUpdate, ViewPlugin, EditorView, Decoration, DecorationSet } from '@codemirror/view';
import { Extension } from '@codemirror/state';
import { extractEpistemicEntities, EpistemicEntity } from './nlpEntityRecognition';
import {
	createEntityAnalysisField,
	EntityAnalysisState,
	analyzeDocumentEffect,
	updateEntitiesEffect,
	createAnalysisEffect,
	createEntitiesEffect
} from './editorStateField';
import { buildDecorations } from './editorDecorations';

interface NLPSettings {
	enabled: boolean;
	showBadges: boolean;
	debounceMs: number;
	realTimeAnalysis: boolean;
}

const defaultSettings: NLPSettings = {
	enabled: true,
	showBadges: false,
	debounceMs: 500,
	realTimeAnalysis: true
};

let debounceTimer: number | null = null;

/**
 * Analyze document text and update entities
 */
async function analyzeDocument(
	view: EditorView,
	state: EntityAnalysisState,
	settings: NLPSettings
): Promise<void> {
	const text = view.state.doc.sliceString(0);
	
	if (text === state.text && state.entities.length > 0) {
		// No need to re-analyze
		return;
	}

	try {
		const entities = extractEpistemicEntities(text);
		
		view.dispatch({
			effects: [updateEntitiesEffect.of({ entities })]
		});
	} catch (error) {
		console.error('Error analyzing document:', error);
	}
}

/**
 * Debounced analysis function
 */
function debouncedAnalysis(
	view: EditorView,
	state: EntityAnalysisState,
	settings: NLPSettings
): void {
	if (debounceTimer !== null) {
		clearTimeout(debounceTimer);
	}

	debounceTimer = window.setTimeout(() => {
		const text = view.state.doc.sliceString(0);
		view.dispatch({
			effects: [createAnalysisEffect(text)]
		});
		analyzeDocument(view, state, settings);
		debounceTimer = null;
	}, settings.debounceMs);
}

/**
 * Create the editor view plugin
 */
function createNLPViewPlugin(settings: NLPSettings = defaultSettings) {
	return ViewPlugin.fromClass(
		class {
			field: EntityAnalysisState;
			decorations: DecorationSet;

			constructor(view: EditorView) {
				this.field = view.state.field(createEntityAnalysisField());
				this.decorations = Decoration.none;
				this.updateDecorations(view);
			}

			update(update: ViewUpdate) {
				const newField = update.state.field(createEntityAnalysisField());
				const changed = newField !== this.field || update.docChanged || update.viewportChanged;

				if (changed) {
					this.field = newField;

					// Trigger analysis if enabled and document changed
					if (settings.enabled && settings.realTimeAnalysis && update.docChanged) {
						debouncedAnalysis(update.view, this.field, settings);
					}

					this.updateDecorations(update.view);
				}
			}

			updateDecorations(view: EditorView) {
				if (!settings.enabled || this.field.isAnalyzing) {
					this.decorations = Decoration.none;
					return;
				}

				this.decorations = buildDecorations(
					view,
					this.field.entities,
					settings.showBadges
				);
			}

			destroy() {
				if (debounceTimer !== null) {
					clearTimeout(debounceTimer);
					debounceTimer = null;
				}
			}
		},
		{
			decorations: (v) => v.decorations
		}
	);
}

/**
 * Create CodeMirror extension for NLP entity analysis
 */
export function createNLPExtension(settings: NLPSettings = defaultSettings): Extension {
	return [
		createEntityAnalysisField(),
		createNLPViewPlugin(settings)
	];
}

/**
 * Update settings for NLP plugin
 */
export function updateNLPSettings(
	view: EditorView,
	newSettings: Partial<NLPSettings>
): void {
	// Settings updates require recreating the extension
	// This is handled at the plugin level
	console.log('NLP settings updated:', newSettings);
}

