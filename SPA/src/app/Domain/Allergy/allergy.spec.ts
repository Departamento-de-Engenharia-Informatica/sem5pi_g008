import {Allergy} from './Allergy';

describe('Allergy Domain Tests', () => {
  let allergy: Allergy;

  beforeEach(() => {

    allergy = {
      domainId: 1,
      code: 'AL10',
      designation: 'TEST_ALLERGY',
      description: 'This is a test allergy',
      effects: ['Rash', 'Itching'],
      isDeleted: false,
    };
  });

  it('should create an Allergy object successfully', () => {
    expect(allergy).toBeDefined();
    expect(allergy.domainId).toBe(1);
    expect(allergy.code).toBe('AL10');
    expect(allergy.designation).toBe('TEST_ALLERGY');
    expect(allergy.description).toBe('This is a test allergy');
    expect(allergy.effects).toEqual(['Rash', 'Itching']);
    expect(allergy.isDeleted).toBe(false);
  });
});
