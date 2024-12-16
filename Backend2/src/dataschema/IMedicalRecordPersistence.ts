import {Allergy} from "../domain/Allergy/Allergy";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";

export interface IMedicalRecordPersistence{
    domainId: number;
    allergies: Allergy[];
    medicalConditions: MedicalCondition[];
    freeText?: string;
}