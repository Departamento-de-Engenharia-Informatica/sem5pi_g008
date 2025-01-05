import {Mapper} from "../core/infra/Mapper";
import mongoose, {Document, Model} from "mongoose";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {MedicalRecordCondition} from "../domain/MedicalRecordCondition/MedicalRecordCondition";
import IMedicalRecordConditionDTO from "../dto/IMedicalRecordConditionDTO";
import {IMedicalRecordConditionPersistence} from "../dataschema/IMedicalRecordConditionPersistence";
import IStaffDetailsDTO from "../dto/IStaffDetailsDTO";
import {MedicalCondition} from "../domain/MedicalCondition/MedicalCondition";
import {cond} from "lodash";

export class MedicalRecordConditionMapper extends Mapper<MedicalRecordCondition> {

  public static toDTO(domain: any, condition?: MedicalCondition, medicalRecordBusinessId?: string, staffDetailsDTO?: IStaffDetailsDTO): IMedicalRecordConditionDTO {
    
    if (condition && medicalRecordBusinessId) {
      
      if(staffDetailsDTO) {
        return {
          conditionId: condition.id.toString(),
          conditionCode: condition.code.value,
          conditionDesignation: condition.designation.value,
          medicalRecordId: medicalRecordBusinessId.toString(),
          doctorName: staffDetailsDTO.firstName + ' ' + staffDetailsDTO.lastName, 
          doctorLicenseNumber: staffDetailsDTO.licenseNumber,
          comment: domain.comment,
          domainId: domain.domainId
        } as IMedicalRecordConditionDTO;
      }
      
      return {
        conditionId: condition.id.toString(),
        conditionCode: condition.code.value,
        conditionDesignation: condition.designation.value,
        medicalRecordId: medicalRecordBusinessId.toString(),
        doctorName: "Unknown",
        comment: domain.comment
        domainId: domain.domainId
      } as IMedicalRecordConditionDTO;
    }

    return {
      conditionId: domain.condition,
      medicalRecordId: domain.medicalRecord,
      doctorId: domain.doctorId,
      comment: domain.comment,
      domainId: domain.domainId
    } as IMedicalRecordConditionDTO;
  }

  public static toDomain (medicalRecordCondition: any | Model<IMedicalRecordConditionPersistence & Document> ): MedicalRecordCondition {
    
    const medicalRecordConditionResult = MedicalRecordCondition.create(
      medicalRecordCondition,
      new UniqueEntityID(medicalRecordCondition.domainId)
    );

    medicalRecordConditionResult.isFailure ? console.log(medicalRecordConditionResult.error) : '';

    return medicalRecordConditionResult.isSuccess ? medicalRecordConditionResult.getValue() : null;
  }

  public static toPersistence (medicalRecordCondition: MedicalRecordCondition, id?: number): IMedicalRecordConditionPersistence {
    if(id === undefined) {
      return {
        domainId: medicalRecordCondition.domainId,
        conditionId: new mongoose.Types.ObjectId(medicalRecordCondition.condition),
        medicalRecordId: new mongoose.Types.ObjectId(medicalRecordCondition.medicalRecord),
        comment: medicalRecordCondition.comment,
        doctorId: medicalRecordCondition.doctorId,
      }
    }

    return {
      domainId: id,
      conditionId: new mongoose.Types.ObjectId(medicalRecordCondition.condition),
      medicalRecordId: new mongoose.Types.ObjectId(medicalRecordCondition.medicalRecord),
      comment: medicalRecordCondition.comment,
      doctorId: medicalRecordCondition.doctorId,
    }
  }
  
  
}
