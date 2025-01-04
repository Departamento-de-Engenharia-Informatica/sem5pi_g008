import { MedicalCondition } from "../../../../src/domain/MedicalCondition/MedicalCondition";
import { Designation } from "../../../../src/domain/Shared/designation";
import { Code } from "../../../../src/domain/Shared/code";
import { Description } from "../../../../src/domain/Shared/description";

describe("MedicalCondition with valid and invalid cases", () => {
    let validCode: Code;
    let validDesignation: Designation;
    let validDescription: Description;
    let validSymptomsList: string[];

    beforeEach(() => {
        validCode = Code.create("M123").getValue();
        validDesignation = Designation.create("Diabetes").getValue();
        validDescription = Description.create("A chronic condition affecting blood sugar.").getValue();
        validSymptomsList = ["Increased thirst", "Frequent urination", "Extreme hunger"];
    });

    it("should create a MedicalCondition instance successfully with valid properties", () => {
        const props = {
            code: validCode,
            designation: validDesignation,
            description: validDescription,
            symptomsList: validSymptomsList,
        };

        const result = MedicalCondition.create(props);
        expect(result.isSuccess).toBe(true);

        const medicalCondition = result.getValue();
        expect(medicalCondition).toBeInstanceOf(MedicalCondition);
        expect(medicalCondition.code).toEqual(validCode);
        expect(medicalCondition.designation).toEqual(validDesignation);
        expect(medicalCondition.description).toEqual(validDescription);
        expect(medicalCondition.symptomsList).toEqual(validSymptomsList);
    });
});
