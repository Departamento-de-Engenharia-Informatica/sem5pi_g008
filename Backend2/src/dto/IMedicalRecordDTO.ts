import IMedicalRecordFreeTextDTO from "./IMedicalRecordFreeTextDTO";
import IMedicalRecordConditionDTO from "./IMedicalRecordConditionDTO";
import IMedicalRecordAllergyDTO from "./IMedicalRecordAllergyDTO";

export default interface IMedicalRecordDTO{
    domainId?:number;
    medicalRecordConditions?: IMedicalRecordConditionDTO[];
    medicalRecordAllergies?: IMedicalRecordAllergyDTO[];
    freeText?: IMedicalRecordFreeTextDTO[];
}
