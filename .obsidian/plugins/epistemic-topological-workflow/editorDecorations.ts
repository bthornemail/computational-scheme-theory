/**
 * Editor Decorations - CodeMirror decorations for epistemic entities
 */

import { Decoration, DecorationSet, EditorView, WidgetType } from '@codemirror/view';
import { Range } from '@codemirror/state';
import { EpistemicEntity, EpistemicEntityType } from './nlpEntityRecognition';

/**
 * Widget for entity badges/labels
 */
class EntityBadgeWidget extends WidgetType {
	constructor(
		private entityType: EpistemicEntityType,
		private text: string
	) {
		super();
	}

	toDOM(): HTMLElement {
		const span = document.createElement('span');
		span.className = `epistemic-entity-badge epistemic-entity-${this.entityType}`;
		span.textContent = this.getLabel(this.entityType);
		span.title = `${this.getLabel(this.entityType)}: ${this.text}`;
		span.style.marginLeft = '2px';
		span.style.fontSize = '0.7em';
		span.style.opacity = '0.7';
		return span;
	}

	ignoreEvent(): boolean {
		return true;
	}

	private getLabel(type: EpistemicEntityType): string {
		switch (type) {
			case 'known-known': return '✓';
			case 'known-unknown': return '?';
			case 'unknown-known': return '~';
			case 'unknown-unknown': return '??';
			case 'transitional': return '→';
			case 'meta-knowledge': return '📚';
			case 'foundational': return '★';
			case 'mathematical-concept': return '∑';
			case 'workflow-stage': return '⚙';
			default: return '•';
		}
	}
}

/**
 * Get CSS class for epistemic entity type
 */
function getEntityClass(type: EpistemicEntityType): string {
	return `epistemic-entity-${type}`;
}

/**
 * Get decoration mark for highlighting
 */
function getMarkDecoration(type: EpistemicEntityType): Decoration {
	return Decoration.mark({
		class: `epistemic-entity-mark ${getEntityClass(type)}`,
		attributes: {
			title: getEntityLabel(type)
		}
	});
}

/**
 * Get decoration widget for badge
 */
function getWidgetDecoration(type: EpistemicEntityType, text: string): Decoration {
	return Decoration.widget({
		widget: new EntityBadgeWidget(type, text),
		side: 1
	});
}

/**
 * Get human-readable label for entity type
 */
function getEntityLabel(type: EpistemicEntityType): string {
	switch (type) {
		case 'known-known': return 'Known Known';
		case 'known-unknown': return 'Known Unknown';
		case 'unknown-known': return 'Unknown Known';
		case 'unknown-unknown': return 'Unknown Unknown';
		case 'transitional': return 'Transitional';
		case 'meta-knowledge': return 'Meta-knowledge';
		case 'foundational': return 'Foundational';
		case 'mathematical-concept': return 'Mathematical Concept';
		case 'workflow-stage': return 'Workflow Stage';
		default: return 'Entity';
	}
}

/**
 * Build decoration set from entities
 */
export function buildDecorations(
	view: EditorView,
	entities: EpistemicEntity[],
	showBadges: boolean = false
): DecorationSet {
	const decorations: Range<Decoration>[] = [];

	entities.forEach(entity => {
		// Ensure positions are within document bounds
		const docLength = view.state.doc.length;
		const start = Math.max(0, Math.min(entity.start, docLength));
		const end = Math.max(start, Math.min(entity.end, docLength));

		if (start < end) {
			// Add mark decoration for highlighting
			const markDeco = getMarkDecoration(entity.type);
			decorations.push(markDeco.range(start, end));

			// Optionally add widget decoration for badge
			if (showBadges) {
				const widgetDeco = getWidgetDecoration(entity.type, entity.text);
				decorations.push(widgetDeco.range(end));
			}
		}
	});

	return Decoration.set(decorations);
}

