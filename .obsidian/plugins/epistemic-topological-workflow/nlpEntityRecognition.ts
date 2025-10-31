/**
 * NLP Entity Recognition - Using wink-nlp to detect epistemic entities in text
 */

import winkNLP from 'wink-nlp';
import model from 'wink-eng-lite-web-model';

// Initialize NLP with English lite web model
const nlp = winkNLP(model);
const its = nlp.its;
const as = nlp.as;

export type EpistemicEntityType = 
	| 'known-known'
	| 'known-unknown'
	| 'unknown-known'
	| 'unknown-unknown'
	| 'transitional'
	| 'meta-knowledge'
	| 'foundational'
	| 'mathematical-concept'
	| 'workflow-stage';

export interface EpistemicEntity {
	type: EpistemicEntityType;
	text: string;
	start: number;
	end: number;
	confidence?: number;
}

// Define custom entity patterns for epistemic states
const epistemicPatterns = [
	// Known Known / Known Knowns
	{
		name: 'known-known',
		patterns: [
			'known known',
			'known knowns',
			'validated knowledge',
			'complete knowledge',
			'implemented and tested',
			'formalized specification'
		]
	},
	// Known Unknown / Known Unknowns
	{
		name: 'known-unknown',
		patterns: [
			'known unknown',
			'known unknowns',
			'TODO',
			'FIXME',
			'placeholder',
			'planned feature',
			'knowledge gap',
			'recognized gap'
		]
	},
	// Unknown Known / Unknown Knowns
	{
		name: 'unknown-known',
		patterns: [
			'unknown known',
			'unknown knowns',
			'implicit knowledge',
			'undocumented',
			'tribal knowledge',
			'assumption'
		]
	},
	// Unknown Unknown / Unknown Unknowns
	{
		name: 'unknown-unknown',
		patterns: [
			'unknown unknown',
			'unknown unknowns',
			'unrecognized gap',
			'emergent complexity',
			'undiscovered issue',
			'risk'
		]
	},
	// Transitional
	{
		name: 'transitional',
		patterns: [
			'in-progress',
			'in progress',
			'uncertain',
			'draft',
			'work in progress'
		]
	},
	// Meta-knowledge
	{
		name: 'meta-knowledge',
		patterns: [
			'documentation',
			'guidance',
			'explanation',
			'guide'
		]
	},
	// Foundational
	{
		name: 'foundational',
		patterns: [
			'core theory',
			'axiom',
			'foundation',
			'fundamental',
			'principles'
		]
	},
	// Mathematical concepts
	{
		name: 'mathematical-concept',
		patterns: [
			'H¹',
			'H1',
			'cohomology',
			'V\\(G\\)',
			'cyclomatic complexity',
			'Grothendieck scheme',
			'Zariski topology',
			'Čech complex',
			'rig',
			'semiring',
			'commutative algebra',
			'homology',
			'FSM',
			'finite state machine'
		]
	},
	// Workflow stages
	{
		name: 'workflow-stage',
		patterns: [
			'research',
			'analysis',
			'assessment',
			'reflection',
			'formalization',
			'specification',
			'proposal',
			'implementation',
			'validation',
			'documentation',
			'guidance'
		]
	}
];

// Learn custom entities
let entitiesLearned = false;

export function initializeNLPCustomEntities(): void {
	if (entitiesLearned) return;

	try {
		const patterns = epistemicPatterns.map(p => ({
			name: p.name,
			patterns: p.patterns
		}));

		nlp.learnCustomEntities(patterns, {});
		entitiesLearned = true;
	} catch (error) {
		console.error('Failed to learn custom entities:', error);
	}
}

/**
 * Analyze text and extract epistemic entities
 */
export function extractEpistemicEntities(text: string): EpistemicEntity[] {
	if (!entitiesLearned) {
		initializeNLPCustomEntities();
	}

	const entities: EpistemicEntity[] = [];

	try {
		const doc = nlp.readDoc(text);
		
		// Extract custom entities using wink-nlp API
		doc.entities().each((entity: any) => {
			const entityType = entity.type() as string;
			if (isValidEpistemicType(entityType)) {
				const span = entity.span() as string;
				const spanStart = entity.spanStart() as number;
				const spanEnd = entity.spanEnd() as number;
				if (span && spanStart >= 0 && spanEnd > spanStart) {
					entities.push({
						type: entityType as EpistemicEntityType,
						text: span,
						start: spanStart,
						end: spanEnd,
						confidence: 0.8 // Default confidence for pattern-based matching
					});
				}
			}
		});

		// Also search for keywords directly (fallback)
		epistemicPatterns.forEach(pattern => {
			pattern.patterns.forEach(patternText => {
				const regex = new RegExp(patternText.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'), 'gi');
				let match: RegExpExecArray | null;
				while ((match = regex.exec(text)) !== null) {
					if (match && match.index !== undefined && match[0]) {
						// Check if not already found
						const alreadyFound = entities.some(e => 
							e.start <= match!.index && e.end >= match!.index + match![0].length
						);
						if (!alreadyFound) {
							entities.push({
								type: pattern.name as EpistemicEntityType,
								text: match[0],
								start: match.index,
								end: match.index + match[0].length,
								confidence: 0.7
							});
						}
					}
				}
			});
		});

		// Sort by position
		entities.sort((a, b) => a.start - b.start);

		// Remove overlapping entities (keep first, longer, or higher confidence)
		const filteredEntities: EpistemicEntity[] = [];
		entities.forEach(entity => {
			const overlaps = filteredEntities.filter(e => 
				(entity.start < e.end && entity.end > e.start)
			);
			if (overlaps.length === 0) {
				filteredEntities.push(entity);
			} else {
				// Replace if this entity is better
				const shouldReplace = overlaps.some(overlap => {
					const entityConf = entity.confidence || 0.5;
					const overlapConf = overlap.confidence || 0.5;
					return entityConf > overlapConf ||
						(entity.end - entity.start) > (overlap.end - overlap.start);
				});
				if (shouldReplace) {
					// Remove overlapping entities
					overlaps.forEach(overlap => {
						const index = filteredEntities.indexOf(overlap);
						if (index > -1) filteredEntities.splice(index, 1);
					});
					filteredEntities.push(entity);
				}
			}
		});

		return filteredEntities.sort((a, b) => a.start - b.start);
	} catch (error) {
		console.error('Error extracting entities:', error);
		return [];
	}
}

function isValidEpistemicType(type: string): type is EpistemicEntityType {
	return [
		'known-known',
		'known-unknown',
		'unknown-known',
		'unknown-unknown',
		'transitional',
		'meta-knowledge',
		'foundational',
		'mathematical-concept',
		'workflow-stage'
	].includes(type);
}

/**
 * Initialize NLP on module load
 */
initializeNLPCustomEntities();

