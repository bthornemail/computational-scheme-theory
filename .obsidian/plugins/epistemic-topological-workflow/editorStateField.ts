/**
 * Editor State Field - CodeMirror state management for NLP entity analysis
 */

import { StateField, StateEffect, Transaction } from '@codemirror/state';
import { EpistemicEntity } from './nlpEntityRecognition';

/**
 * State effect to trigger document analysis
 */
export const analyzeDocumentEffect = StateEffect.define<{ text: string }>();

/**
 * State effect to update entities
 */
export const updateEntitiesEffect = StateEffect.define<{ entities: EpistemicEntity[] }>();

/**
 * State effect to clear analysis
 */
export const clearAnalysisEffect = StateEffect.define();

/**
 * State value for entity analysis
 */
export interface EntityAnalysisState {
	entities: EpistemicEntity[];
	text: string;
	lastModified: number;
	isAnalyzing: boolean;
}

/**
 * Create the state field for entity analysis
 */
export function createEntityAnalysisField(): StateField<EntityAnalysisState> {
	return StateField.define<EntityAnalysisState>({
		create(): EntityAnalysisState {
			return {
				entities: [],
				text: '',
				lastModified: Date.now(),
				isAnalyzing: false
			};
		},
		update(value: EntityAnalysisState, tr: Transaction): EntityAnalysisState {
			let newValue = value;

			// Handle effects
			for (const effect of tr.effects) {
				if (effect.is(analyzeDocumentEffect)) {
					newValue = {
						...newValue,
						text: effect.value.text,
						isAnalyzing: true,
						lastModified: Date.now()
					};
				} else if (effect.is(updateEntitiesEffect)) {
					newValue = {
						...newValue,
						entities: effect.value.entities,
						isAnalyzing: false
					};
				} else if (effect.is(clearAnalysisEffect)) {
					newValue = {
						entities: [],
						text: '',
						lastModified: Date.now(),
						isAnalyzing: false
					};
				}
			}

			// Update text if document changed and not from effect
			if (tr.docChanged && !tr.effects.some(e => e.is(analyzeDocumentEffect))) {
				const newText = tr.newDoc.sliceString(0);
				if (newText !== newValue.text) {
					newValue = {
						...newValue,
						text: newText,
						lastModified: Date.now()
					};
				}
			}

			return newValue;
		}
	});
}

/**
 * Helper to dispatch analysis effect
 * Note: These helpers are for use within transactions, not standalone
 */
export function createAnalysisEffect(text: string) {
	return analyzeDocumentEffect.of({ text });
}

/**
 * Helper to dispatch entity update
 */
export function createEntitiesEffect(entities: EpistemicEntity[]) {
	return updateEntitiesEffect.of({ entities });
}

/**
 * Helper to clear analysis
 */
export function createClearAnalysisEffect() {
	return clearAnalysisEffect.of(null);
}

