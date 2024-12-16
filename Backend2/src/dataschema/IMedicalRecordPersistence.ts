import {Allergy} from "../domain/Allergy/Allergy";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import {MedicalRecordAllergy} from "../domain/MedicalRecordAllergy/MedicalRecordAllergy";
import {MedicalRecordFreeText} from "../domain/MedicalRecordFreeText/MedicalRecordFreeText";

export interface IMedicalRecordPersistence{
  domainId: string;
  medicalRecordConditions: MedicalRecordCondition[];
  medicalRecordAllergies: MedicalRecordAllergy[];
  freeText: MedicalRecordFreeText[];
}
