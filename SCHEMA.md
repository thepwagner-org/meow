# Schema Format

Custom markdown document schemas for validation and structure enforcement.

## Overview

Schemas are YAML files in `.meow.d/` directories that define document types. Each `{type}.yaml` file specifies structure rules, frontmatter fields, sections, and link constraints for markdown documents.

**Purpose:**
- Validate document structure (H1, sections, frontmatter)
- Enforce link integrity and bidirectional relationships
- Auto-generate index files for document collections
- Provide templates and guidance for content creation

**Discovery:**
meow searches parent directories for `.meow.d/` and loads all `*.yaml` files. Documents declare their type via frontmatter `type: typename`.

## Quick Start

**Simple schema** (`.meow.d/restaurant.yaml`):
```yaml
description: Restaurant or dining establishment
structure:
  title_from_filename: true
  intro:
    description: Type of cuisine and location
  sections:
    - title: Visits
      description: Visit history with dates and notes
    - title: Favourites
      description: Recommended dishes
```

**Corresponding document** (`restaurants/Sushi Place.md`):
```markdown
---
type: restaurant
---
# Sushi Place

- Japanese cuisine downtown
- Authentic omakase experience

## Visits

- 2024-01-15 - Amazing toro, sat at the bar
- 2024-03-20 - Brought friends, everyone loved it

## Favourites

- Otoro nigiri
- Spicy salmon hand roll
```

## Schema Properties

### Top-Level Keys

```yaml
description: string           # Human-readable description of this document type
index: string                 # Optional: path to auto-generated index file (e.g., "people/README.md")
fields: {}                    # Optional: typed field validation (enum, date, integer, etc.)
structure:                    # Document structure definition (required)
  title_from_filename: bool   # H1 must match filename without .md extension
  frontmatter: []             # Frontmatter presence checks with LLM descriptions
  intro: {}                   # Intro section (between H1 and first H2)
  sections: []                # H2 section definitions
```

### Structure Definition

#### Title Validation

```yaml
structure:
  title_from_filename: true   # H1 must match filename (without .md)
```

When enabled, `file.md` must have `# file` as its H1. Spaces and special characters in filenames are preserved.

#### Frontmatter Fields

`structure.frontmatter` checks field **presence** and provides LLM guidance. It does not validate types or values — use the top-level `fields` key for that (see [Typed Fields](#typed-fields)).

```yaml
structure:
  frontmatter:
    - name: field_name        # YAML frontmatter key (required)
      description: "..."      # Optional: guidance for LLMs and documentation
      required: true          # Default: false
```

**Example:**
```yaml
structure:
  frontmatter:
    - name: scope
      description: "family | friend | professional"
    - name: notes
      description: Free-text notes about this person
      required: true
```

#### Typed Fields

The top-level `fields` key provides actual type validation for frontmatter values. Use this when you need enum constraints, type checking, or link validation on frontmatter fields.

```yaml
fields:
  field_name:
    type: string              # string | date | datetime | integer | bool | enum | link | list
    required: true            # Default: false
    values:                   # Required for enum (and list with item_type: enum)
      - value1
      - value2
    item_type: string         # For list fields: type of each item
```

**Supported types:**

| Type | Description |
|------|-------------|
| `string` | Any text value |
| `date` | Normalized to YYYY-MM-DD; accepts flexible input (2024-03-15, March 15 2024, etc.) |
| `datetime` | ISO 8601 with timezone; accepts flexible input |
| `integer` | Whole number |
| `bool` | Boolean true/false |
| `enum` | One of the values in `values` (case-sensitive, exact match) |
| `link` | File path; validated for existence on disk |
| `list` | Array; optional `item_type` validates each element |

**Example with enums:**
```yaml
fields:
  likelihood:
    type: enum
    required: true
    values: [high, medium, low]
  impact:
    type: enum
    required: true
    values: [high, medium, low]
```

`fields` and `structure.frontmatter` can coexist — use `structure.frontmatter` for LLM descriptions and presence checks, `fields` for type validation.

#### Intro Section

Content between H1 and first H2:

```yaml
structure:
  intro:
    description: "Purpose of intro content"  # LLM guidance
    template: "- Example format"             # Format hint (not enforced for intro)
```

Intro templates are documentation only; actual content is freeform. (Section templates **are** enforced — see below.)

#### Sections

H2-level sections with validation rules. Only sections listed in the schema are allowed — unlisted H2 headings cause a validation error.

```yaml
structure:
  sections:
    - title: "Section Name"   # Exact H2 heading text (required)
      description: "..."      # Purpose and expected content (optional)
      required: true          # Must be present (default: false)
      paragraph: true         # Allow prose (default: false = bullets only)
      template: "- Format"    # Validated against list items (optional)
      links:                  # Link validation rules (optional)
        target_type: typename # Links must point to documents of this type
        bidirectional: true   # Validate backlinks exist (default: false)
```

When `paragraph` is false (the default), only bullet lists are allowed. When a `template` is defined, list items are validated against the template pattern.

**Section Properties:**

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `title` | string | (required) | Exact H2 heading text |
| `description` | string | null | LLM guidance on content purpose |
| `required` | bool | false | Section must be present |
| `paragraph` | bool | false | Allow paragraphs; false = bullets only |
| `template` | string | null | Pattern validated against list items |
| `links` | object | null | Link validation constraints |

**Link Properties:**

| Property | Type | Default | Description |
|----------|------|---------|-------------|
| `target_type` | string | null | Document type that links must target |
| `bidirectional` | bool | false | Validate backlinks exist in target documents |

## Common Patterns

### Simple Document

Minimal structure with title, intro, and basic sections:

```yaml
description: A musical artist or band
structure:
  title_from_filename: true
  intro:
    description: Who they are, your relationship to their music
    template: "- Fact or impression about the artist"
  sections:
    - title: Discography
      description: Albums with ### subheadings (chronological), your notes on each
    - title: Top Tracks
      description: Favorite or most played songs
    - title: History
      description: When you discovered them, saw them live, etc
      template: "- YYYY - discovered / saw live / etc"
```

### Document with Typed Fields

Structured metadata with constrained values using the top-level `fields` key:

```yaml
description: An investment security or fund
index: securities/README.md
fields:
  ticker:
    type: string
    required: true
  asset_class:
    type: enum
    required: true
    values:
      - equity
      - fixed-income
      - balanced
      - real-estate
      - commodity
      - cash
      - cryptocurrency
  currency:
    type: enum
    required: true
    values: [CAD, USD, EUR, BTC]
structure:
  title_from_filename: true
  intro:
    description: Investment objective and key facts
  sections:
    - title: Overview
      description: Exchange, inception, assets, fees
      required: true
```

**Example document:**
```markdown
---
type: security
ticker: VBAL
asset_class: balanced
currency: CAD
---
# VBAL

- Vanguard Balanced ETF Portfolio
- 60/40 equity/fixed-income allocation

## Overview

- Exchange: TSX
- Inception: 2018-01-29
- MER: 0.24%
```

### Document with Bidirectional Links

Enforce symmetric relationships between documents:

```yaml
description: A person reference document
index: people/README.md
structure:
  title_from_filename: true
  frontmatter:
    - name: scope
      description: "Relationship categories: family, friend, professional"
      required: false
  intro:
    description: Key facts about this person
    template: "- Fact about the person"
  sections:
    - title: Relationships
      description: Links to related people
      template: "- [Name](Name.md) - relationship type"
      required: true
      links:
        target_type: person
        bidirectional: true
    - title: Interests
      description: Links to interest documents
      template: "- [Interest](../interests/Interest.md)"
      links:
        target_type: interest
```

**Example documents:**
```markdown
---
type: person
scope: friend
---
# Jane Smith

- Friend from university
- Studied Computer Science

## Relationships

- [John Doe](John%20Doe.md) - friend from university
- [Alice Johnson](Alice%20Johnson.md) - housemate at university

## Interests

- [Game Development](../interests/Game%20Development.md)
```

With `bidirectional: true`, if Jane's document links to John, then John's document **must** link back to Jane. Validation fails if backlinks are missing.

### Threat Modeling Pattern

Security analysis with frontmatter risk ratings and cross-type relationships:

**threat.yaml:**
```yaml
description: An attack vector or failure mode
index: threats/README.md
fields:
  likelihood:
    type: enum
    required: true
    values: [high, medium, low]
  impact:
    type: enum
    required: true
    values: [high, medium, low]
structure:
  title_from_filename: true
  intro:
    description: What this threat is and how it works
  sections:
    - title: Leads To
      description: Threats enabled if this one succeeds
      template: "- [Threat](Threat.md) optional context"
      links:
        target_type: threat
        bidirectional: true
    - title: Mitigated By
      description: Strategies that counter this threat
      template: "- [Mitigation](../mitigations/Mitigation.md) optional context"
      required: true
      links:
        target_type: mitigation
        bidirectional: true
```

**mitigation.yaml:**
```yaml
description: An abstract defensive strategy
index: mitigations/README.md
structure:
  title_from_filename: true
  intro:
    description: What this strategy accomplishes
  sections:
    - title: Counters
      description: Threats this mitigation addresses
      template: "- [Threat](../threats/Threat.md) optional context"
      required: true
      links:
        target_type: threat
        bidirectional: true
    - title: Implemented By
      description: Controls that realize this strategy
      template: "- [Control](../controls/Control.md) optional context"
      links:
        target_type: control
        bidirectional: true
```

**Example threat document:**
```markdown
---
type: threat
likelihood: medium
impact: high
---
# Phishing

- Attacker tricks user into revealing credentials via fake login pages
- Bypasses password strength entirely - user hands over valid credentials

## Leads To

- [Account Takeover](Account%20Takeover.md) - attacker has valid credentials

## Mitigated By

- [Multi-Factor Authentication](../mitigations/Multi-Factor%20Authentication.md)
- [Unique Password](../mitigations/Unique%20Password.md)
```

This creates a bidirectional graph: threats link to mitigations, mitigations link back to threats, enabling consistency validation across the threat model.

### Complex Multi-Type Relationships

Hub document linking to many specialized types:

```yaml
description: A topic, franchise, or area of interest
structure:
  title_from_filename: true
  intro:
    description: Why this interest is compelling
    template: "- What makes this interesting"
  sections:
    - title: TV Shows
      description: Related TV shows
      template: "- [Show](../tvshows/Show.md) - context"
      links:
        target_type: tvshow
    - title: Games
      description: Related games
      template: "- [Game](../games/Game.md) - context"
      links:
        target_type: game
    - title: Bands
      description: Related bands/artists
      template: "- [Band](../bands/Band.md) - context"
      links:
        target_type: band
    - title: Hardware
      description: Related hardware
      template: "- [Device](../hardwares/Device.md) - context"
      links:
        target_type: hardware
    - title: Related Interests
      description: Links to overlapping or connected interests
      template: "- [Interest](Interest.md) - how they connect"
      links:
        target_type: interest
        bidirectional: true
```

This pattern creates a knowledge graph where interests aggregate related media and concepts, with bidirectional links between overlapping interests.

## Index Generation

When `index: path/to/README.md` is specified, meow auto-generates a listing of all documents of that type:

```yaml
description: A person reference document
index: people/README.md
structure:
  # ...
```

The system maintains `people/README.md` with a generated list of all person documents. This file is auto-updated and should not be manually edited.

## Using Schemas in Documents

### Document Frontmatter

Every document using a schema must declare its type:

```yaml
---
type: typename
# Additional frontmatter fields as defined by schema
---
```

The `type` field must match a schema filename (without `.yaml`) in the nearest `.meow.d/` directory.

### Structure Compliance

Documents must follow schema-defined structure:

1. **Title**: If `title_from_filename: true`, H1 must match filename exactly
2. **Frontmatter**: Required fields must be present (`structure.frontmatter`); typed fields must pass validation (`fields`)
3. **Intro**: Content between H1 and first H2 (if schema defines intro section); respects `paragraph` mode
4. **Sections**: Required sections must be present; order is enforced; unlisted H2 sections are rejected
5. **Templates**: List items in sections are validated against `template` patterns (when defined and `paragraph: false`)
6. **Links**: Must target correct types; bidirectional links must exist in both documents

### Example: Complete Person Document

**Schema** (`.meow.d/person.yaml`):
```yaml
description: A person reference document
index: people/README.md
structure:
  title_from_filename: true
  frontmatter:
    - name: scope
      description: "family | friend | professional"
  intro:
    description: Key facts about this person
  sections:
    - title: Relationships
      required: true
      links:
        target_type: person
        bidirectional: true
```

**Document** (`people/Jane Smith.md`):
```markdown
---
type: person
scope: friend
---
# Jane Smith

- Friend from university
- Works in software engineering

## Relationships

- [John Doe](John%20Doe.md) - friend from university
- [Alice Johnson](Alice%20Johnson.md) - housemate at university
```

**Validation enforces:**
- H1 is exactly "Jane Smith" (matches filename)
- Frontmatter has `type: person` and optional `scope`
- "Relationships" section exists (required)
- All links target `person` type documents
- Linked documents (John, Alice) link back to Jane

## Best Practices

### Creating Schemas

**Start simple:**
- Begin with `title_from_filename` + intro + basic sections
- Add frontmatter for queryable metadata
- Add link validation when relationships are important

**Use required sparingly:**
- Only mark sections `required: true` for critical structure
- Let optional sections emerge organically

**Bidirectional links:**
- Use `bidirectional: true` for symmetric relationships (friendships, dependencies)
- Omit for asymmetric relationships (person → interest, threat → mitigation)

**Templates and descriptions:**
- Provide `template` examples showing expected format
- Write `description` fields to guide LLM content generation
- Section templates are validated against list items (when `paragraph: false`)
- Intro templates are documentation only and not enforced

**Section content modes:**
- Default (`paragraph: false`): bullet lists only
- `paragraph: true`: allow prose paragraphs
- Use bullets for structured lists, paragraphs for notes/analysis

### Writing Documents

**Frontmatter:**
- Always include `type: typename`
- Enum values are case-sensitive and must match exactly (e.g., `high` not `High`)
- Provide all required fields (from both `structure.frontmatter` and `fields`)

**Titles:**
- When `title_from_filename: true`, match H1 to filename exactly
- Preserve spaces, special characters, capitalization

**Links:**
- Follow template format hints
- Ensure bidirectional links exist in both documents
- Use relative paths as shown in templates

**Sections:**
- Include all required sections
- Follow section order as defined in schema
- Respect `paragraph` mode (bullets vs prose)

### Common Pitfalls

**Missing backlinks:**
With `bidirectional: true`, both documents must link to each other:
```markdown
# Alice
## Relationships
- [Bob](Bob.md) - friend

# Bob
## Relationships
- [Alice](Alice.md) - friend  ← Must exist for validation to pass
```

**Enum typos:**
Enum values (from `fields` with `type: enum`) must match exactly:
```yaml
# Schema (top-level fields)
fields:
  likelihood:
    type: enum
    values: [high, medium, low]

# ✓ Correct
likelihood: high

# ✗ Wrong (validation fails)
likelihood: High
likelihood: HIGH
likelihood: med
```

**Title mismatch:**
With `title_from_filename: true`:
```markdown
# Filename: Jane Smith.md
✓ # Jane Smith
✗ # Jane
✗ # jane smith
✗ # Jane S.
```

**Wrong link targets:**
With `target_type: person`:
```markdown
✓ - [Alice](Alice.md)
✓ - [Bob Smith](Bob%20Smith.md)
✗ - [Game Development](../interests/Game%20Development.md)  ← Wrong type
```

## Reference

### Frontmatter Field Properties (`structure.frontmatter`)

Presence checks only — no type or value validation. Use top-level `fields` for that.

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `name` | string | yes | - | YAML frontmatter key |
| `description` | string | no | null | LLM guidance and documentation |
| `required` | bool | no | false | Field must be present |

### Typed Field Properties (`fields`)

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `type` | string | yes | - | `string`, `date`, `datetime`, `integer`, `bool`, `enum`, `link`, `list` |
| `required` | bool | no | false | Field must be present |
| `values` | list | for enum | null | Allowed values (required for `enum` and `list` with `item_type: enum`) |
| `item_type` | string | no | null | For `list` fields: type of each element |

### Section Properties

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `title` | string | yes | - | Exact H2 heading text |
| `description` | string | no | null | Purpose and expected content |
| `required` | bool | no | false | Section must be present |
| `paragraph` | bool | no | false | Allow prose (false = bullets only) |
| `template` | string | no | null | Pattern validated against list items |
| `links` | object | no | null | Link validation rules |

### Link Validation Properties

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `target_type` | string | no | null | Document type links must target |
| `bidirectional` | bool | no | false | Validate backlinks exist |

### Schema Discovery

meow searches for `.meow.d/` directories by walking up from the document's location:

```
project/
  .meow.d/
    person.yaml
    interest.yaml
  people/
    Jane.md          ← Uses ../people/.meow.d/ or ../../.meow.d/
  interests/
    Music.md
```

All `*.yaml` files in the first `.meow.d/` found are loaded as type definitions.

### Document Type Declaration

Documents declare their type via frontmatter:

```yaml
---
type: typename
---
```

The `type` value must match a schema filename (without `.yaml` extension) in the discovered `.meow.d/` directory.
