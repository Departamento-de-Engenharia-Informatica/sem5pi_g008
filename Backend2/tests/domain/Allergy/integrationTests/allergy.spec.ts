import { Designation } from "../../../../src/domain/Shared/designation";
import { Description } from "../../../../src/domain/Shared/description";
import { Allergy } from "../../../../src/domain/Allergy/Allergy";
import { Code } from "../../../../src/domain/Shared/code";

describe('Allergy Domain Test - Integration', () => {
    const initialProps = () => ({
        code: Code.create('A123').getValue(),
        designation: Designation.create('Peanut Allergy').getValue(),
        description: Description.create('Allergic to peanuts causing severe reactions').getValue(),
        effects: ['Anaphylaxis', 'Swelling', 'Hives'],
    });

    it('should create an Allergy entity successfully', () => {
        const allergyProps = initialProps();
        const result = Allergy.create(allergyProps);

        expect(result.isSuccess).toBe(true);
        const allergy = result.getValue();

        expect(allergy.code).toBe('A123');
        expect(allergy.designation).toBe('Peanut Allergy');
        expect(allergy.description).toBe('Allergic to peanuts causing severe reactions');
        expect(allergy.effects).toEqual(['Anaphylaxis', 'Swelling', 'Hives']);
        expect(allergy.isDeleted).toBe(false);
    });

    it('should update the effects of an allergy', () => {
        const allergyProps = initialProps();
        const result = Allergy.create(allergyProps);
        const allergy = result.getValue();

        const newEffects = ['Rashes', 'Difficulty Breathing'];
        allergy.updateEffects(newEffects);

        expect(allergy.effects).toEqual(newEffects);
    });

    it('should update the designation and description of an allergy', () => {
        const allergyProps = initialProps();
        const result = Allergy.create(allergyProps);
        const allergy = result.getValue();

        const newDesignation = Designation.create('Nut Allergy').getValue();
        const newDescription = Description.create('Allergic to nuts causing mild to severe reactions').getValue();

        allergy.designation = newDesignation;
        allergy.description = newDescription;

        expect(allergy.designation).toBe('Nut Allergy');
        expect(allergy.description).toBe('Allergic to nuts causing mild to severe reactions');
    });

    it('should mark isDeleted as false by default', () => {
        const allergyProps = initialProps();
        const result = Allergy.create(allergyProps);
        const allergy = result.getValue();

        expect(allergy.isDeleted).toBe(false);
    });

    it('should handle initialization with isDeleted set to true', () => {
        const allergyProps = { ...initialProps(), isDeleted: true };
        const result = Allergy.create(allergyProps);
        const allergy = result.getValue();

        expect(allergy.isDeleted).toBe(true);
    });

    it('should fail to create an Allergy with missing required properties', () => {
        const incompleteProps: any = {
            designation: Designation.create('Some Allergy').getValue(),
            description: Description.create('Some description').getValue(),
        };
        const result = Allergy.create(incompleteProps);

        expect(result.isFailure).toBe(true);
    });
});
