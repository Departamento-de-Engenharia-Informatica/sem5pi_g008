import { mock, instance, when } from 'ts-mockito';
import { Allergy } from '../../../../src/domain/Allergy/Allergy';
import { Designation } from '../../../../src/domain/Shared/designation';
import { Code } from '../../../../src/domain/Shared/code';
import { Description } from '../../../../src/domain/Shared/description';

describe('Allergy Domain - Unit ', () => {
    let mockCode: Code;
    let mockDesignation: Designation;
    let mockDescription: Description;
    let allergyProps: any;

    beforeEach(() => {
        mockCode = mock(Code);
        mockDesignation = mock(Designation);
        mockDescription = mock(Description);

        when(mockCode.value).thenReturn('A123');
        when(mockDesignation.value).thenReturn('Peanut Allergy');
        when(mockDescription.value).thenReturn('Allergic to peanuts causing severe reactions');

        const codeInstance = instance(mockCode);
        const designationInstance = instance(mockDesignation);
        const descriptionInstance = instance(mockDescription);

        allergyProps = {
            code: codeInstance,
            designation: designationInstance,
            description: descriptionInstance,
            effects: ['Anaphylaxis', 'Swelling', 'Hives'],
        };
    });

    it('should create an Allergy instance successfully', () => {
        const result = Allergy.create(allergyProps);
        expect(result.isSuccess).toBe(true);

        const allergy = result.getValue();
        expect(allergy).toBeInstanceOf(Allergy);
        expect(allergy.code).toEqual(allergyProps.code.value);
        expect(allergy.designation).toEqual(allergyProps.designation.value);
        expect(allergy.description).toEqual(allergyProps.description.value);
        expect(allergy.effects).toEqual(allergyProps.effects);
        expect(allergy.isDeleted).toBe(false); // Default value
    });

    it('should update the effects of an allergy', () => {
        const result = Allergy.create(allergyProps);
        expect(result.isSuccess).toBe(true);

        const allergy = result.getValue();
        const newEffects = ['Rashes', 'Difficulty Breathing'];
        allergy.updateEffects(newEffects);

        expect(allergy.effects).toEqual(newEffects);
    });
    
    it('should handle initialization with isDeleted set to true', () => {
        const propsWithDeleted = { ...allergyProps, isDeleted: true };
        const result = Allergy.create(propsWithDeleted);
        expect(result.isSuccess).toBe(true);

        const allergy = result.getValue();
        expect(allergy.isDeleted).toBe(true);
    });

    it('should update designation and description', () => {
        const result = Allergy.create(allergyProps);
        expect(result.isSuccess).toBe(true);

        const allergy = result.getValue();

        const newMockDesignation = mock(Designation);
        const newMockDescription = mock(Description);
        when(newMockDesignation.value).thenReturn('Nut Allergy');
        when(newMockDescription.value).thenReturn('Allergic to nuts causing mild to severe reactions');

        const newDesignation = instance(newMockDesignation);
        const newDescription = instance(newMockDescription);

        allergy.designation = newDesignation;
        allergy.description = newDescription;

        expect(allergy.designation).toEqual('Nut Allergy');
        expect(allergy.description).toEqual('Allergic to nuts causing mild to severe reactions');
    });
    
});
