import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";
import IMedicalRecordDTO from "../../dto/IMedicalRecordDTO";

import IMedicalRecordFreeTextDTO from "../../dto/IMedicalRecordFreeTextDTO";
import IMedicalRecordConditionDTO from "../../dto/IMedicalRecordConditionDTO";


export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    addFreeText(medicalRecord: IMedicalRecordDTO): Promise<any>;
    getMedicalRecordConditions(medicalRecordId: string): Promise<any>;
    getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<any>;
    getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<any>;

    getFreeTexts(medicalRecordId: string): Promise<IMedicalRecordFreeTextDTO[]>;


    createFamilyHistory(medicalRecordID: any, familyHistory: any): void;
    getAllMedicalRecordConditions():Promise<IMedicalRecordConditionDTO[]>;
    getMedicalRecordFamilyHistoryWithIds(medicalRecordId: any): any;
    
    updateMedicalRecordConditionComment(medicalRecordId: any, updatedComment: any):Promise<any>;

}
